<?php

class HTTP1Request {

    public static function parse(&$stream): HTTP1Request {
        [$method, $url, $version] = parse_first_line($stream);
        $headers = parse_headers($stream);
        $body = parse_body($stream, $headers);
    
        return new HTTP1Request($method, $url, $version, $headers, $body);
    }

    public string $method;
    public string $url;
    public string $version;
    public array $headers;
    public string $body;

    public function __construct($method, $url, $version, $headers, $body) {
        $this->method = $method;
        $this->url = $url;
        $this->version = $version;
        $this->headers = $headers;
        $this->body = $body;
    }

    public function send(&$stream): void {
        if(stream_socket_sendto($stream, "$this->method $this->url $this->version\r\n") === false) throw new Exception("HTTP1Request send stream_socket_sendto failed.");
        foreach($this->headers as &$header) {
            if(stream_socket_sendto($stream, "{$header['key']}: {$header['value']}\r\n") === false) throw new Exception("HTTP1Request send stream_socket_sendto failed.");
        }
        if(stream_socket_sendto($stream, "\r\n") === false) throw new Exception("HTTP1Request send stream_socket_sendto failed.");
        if($this->body && stream_socket_sendto($stream, $this->body) === false) throw new Exception("HTTP1Request send stream_socket_sendto failed.");
    }

    public function print(): void {
        print "\n************************************************ HTTP1Request start\n\n";
        print "$this->method $this->url $this->version\r\n";
        foreach($this->headers as &$header) {
            print $header['key'].": ".$header['value']."\r\n";
        }
        print "\r\n";
        if($this->body) print $this->body."\n";
        print "\n************************************************ HTTP1Request end\n\n";
    }

}

class HTTP1Response {

    public static function parse(&$stream): HTTP1Response {
        [$version, $status_code, $status_text] = parse_first_line($stream);
        $headers = parse_headers($stream);
        $body = parse_body($stream, $headers);

        return new HTTP1Response($version, $status_code, $status_text, $headers, $body);
    }

    public string $version;
    public string $status_code;
    public string $status_text;
    public array $headers;
    public string $body;

    public function __construct($version, $status_code, $status_text, $headers, $body) {
        $this->version = $version;
        $this->status_code = $status_code;
        $this->status_text = $status_text;
        $this->headers = $headers;
        $this->body = $body;
    }

    public function send(&$stream): void {
        if(stream_socket_sendto($stream, "$this->version $this->status_code $this->status_text\r\n") === false) throw new Exception("HTTP1Response send stream_socket_sendto failed.");
        foreach($this->headers as &$header) {
            if(stream_socket_sendto($stream, "{$header['key']}: {$header['value']}\r\n") === false) throw new Exception("HTTP1Response send stream_socket_sendto failed.");
        }
        if(stream_socket_sendto($stream, "\r\n") === false) throw new Exception("HTTP1Response send stream_socket_sendto failed.");
        if($this->body && stream_socket_sendto($stream, $this->body) === false) throw new Exception("HTTP1Response send stream_socket_sendto failed.");
    }

    public function print(): void {
        print "\n************************************************ HTTP1Response start\n\n";
        print "$this->version $this->status_code $this->status_text\r\n";
        foreach($this->headers as &$header) {
            print $header['key'].": ".$header['value']."\r\n";
        }
        print "\r\n";
        if($this->body) print $this->body."\n";
        print "\n************************************************ HTTP1Response end\n\n";
    }

}

function parse_first_line(&$stream): array {
    $first_line = stream_get_line($stream, 0, "\r\n");
    if($first_line === false) throw new Exception("parse_first_line stream_get_line failed.");

    $elements = explode(" ", $first_line);
    if(count($elements) < 3) throw new Exception("parse_first_line invalid first line.");
    $third = implode(" ", array_slice($elements, 2));

    return [$elements[0], $elements[1], $third];
}

function parse_headers(&$stream): array {
    $headers = [];
    while(true) {
        $line = stream_get_line($stream, 0, "\r\n");
        if($line === false) throw new Exception("parse_headers stream_get_line failed.");
        if($line === '') break;
        $key = trim(strstr($line, ":", true));
        $value = trim(substr(strstr($line, ":"), 1));
        array_push($headers, ["key" => $key, "value" => $value]);
    }
    return $headers;
}

function parse_transfer_encoding(&$stream): string {
    $body = "";
    while(true) {
        $chunk_header = stream_get_line($stream, 0, "\r\n");
        if($chunk_header === false) throw new Exception("parse_transfer_encoding chunk_header stream_get_line failed.");

        $body .= $chunk_header."\r\n";

        $hex_str = str_contains($chunk_header, ";") ? strstr($chunk_header, ";", true) : $chunk_header;
        if(!preg_match("/^[a-fA-F0-9]+$/", $hex_str)) throw new Exception("parse_transfer_encoding chunk_header invalid chunk size.");
        $chunk_size = hexdec($hex_str);

        if($chunk_size === 0) {
            while(true) {
                $line = stream_get_line($stream, 0, "\r\n");
                if($line === false) throw new Exception('parse_transfer_encoding trailer or last chunk line(\r\n) stream_get_line failed.');
                $body .= $line."\r\n";
                if($line === '') break;
            }
            break;
        } else {
            $chunk_data = stream_get_contents($stream, $chunk_size);
            if($chunk_data === false) throw new Exception("parse_transfer_encoding chunk_data stream_get_contents failed.");

            $body .= $chunk_data;

            $CRLF = stream_get_line($stream, 0, "\r\n");
            if($CRLF !== '') throw new Exception("parse_transfer_encoding no CRLF after chunk data.");

            $body .= "\r\n";
        }
    }
    return $body;
}

function parse_body(&$stream, array &$headers): string {
    $transfer_encoding = false;
    $content_length = 0;

    foreach($headers as &$header) {
        if(strcasecmp($header["key"], "transfer-encoding") === 0) {
            $transfer_encoding = true;
            break;
        } else if(strcasecmp($header["key"], "content-length") === 0) {
            $content_length = intval($header["value"]);
            if($content_length === 0) throw new Exception("parse_body invalid Content-Length header value");
            break;
        }
    }

    $body = "";
    if($transfer_encoding) {
        $body = parse_transfer_encoding($stream);
    } else if($content_length > 0) {
        $body = stream_get_contents($stream, $content_length);
        if($body === false) throw new Exception("parse_body Content-Length stream_get_contents failed.");
    }

    return $body;
}