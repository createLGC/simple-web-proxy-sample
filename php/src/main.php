<?php

require_once("http.php");
require_once("tls.php");

function tls_relay($src, $dst): void {
    TLSRecord::parse($src)->write($dst);
}

function http_relay($client, $server, ?HTTP1Request $request): void {
    if($request == null) {
        $request = HTTP1Request::parse($client);
    }
    unproxify($request);
    $request->print();
    $request->send($server);
    $response = HTTP1Response::parse($server);
    $response->print();
    $response->send($client);
}

function unproxify(HTTP1Request &$request): void {
    $url_components = parse_url($request->url);
    if($url_components === false) throw new Exception("unproxify parse_url failed.");
    $request->url = ($url_components['path'] ?? "").($url_components['query'] ?? "");
    $request->headers = array_filter($request->headers, fn($header) => strcasecmp(substr($header['key'], 0, 5), "Proxy"), ARRAY_FILTER_USE_BOTH);
}

function start_relay(&$client): void {
    try {
        $request = HTTP1Request::parse($client);
        $url = $request->url;
        $url_components = parse_url($url);
        if($url_components === false) throw new Exception("start_relay parse_url failed.");
        $host = $url_components['host'];
        $port = $url_components['port'] ?? "80";
        $server = stream_socket_client("tcp://$host:$port", $errno, $errstr);
        if(!$server) throw new Exception("start_relay stream_socket_client failed: {$errstr}");
    } catch(Throwable $e) {
        echo "ERROR ".$e->getMessage()."\n";
        $response_body = "<html><body><p>".$e->getMessage()."</p></body></html>";
        $response = new HTTP1Response(
            "HTTP/1.1",
            "400",
            "Bad Request",
            [
                ['key' => "Content-Type", 'value' => "text/html"],
                ['key' => "Content-Length", 'value' => strlen($response_body)],
                ['key' => "Proxy-Connection", 'value' => "close"]
            ],
            $response_body
        );
        $response->send($client);
        stream_socket_shutdown($client, STREAM_SHUT_RDWR);
        return;
    }
    echo "OPEN  $url\n";
    if(strcmp($request->method, "CONNECT") === 0) {
        try {
            $response = new HTTP1Response("HTTP/1.1", "200", "Connection established", [], "");
            $response->send($client);
            while(true) {
                $read = [$client, $server];
                $write = null;
                $except = null;
                $ret = stream_select($read, $write, $except, 3);
                if($ret) {
                    foreach ($read as $stream) {
                        if ($stream === $client) {
                            tls_relay($client, $server);
                        } else if ($stream === $server) {
                            tls_relay($server, $client);
                        }   
                    }  
                }
            }
        } catch(Throwable $e) {
            echo "CLOSE $url ".$e->getMessage()."\n";
            stream_socket_shutdown($server, STREAM_SHUT_RDWR);
            stream_socket_shutdown($client, STREAM_SHUT_RDWR);
        }
    } else {
        $is_first = true;
        try {
            while(true) {
                if($is_first) {
                    http_relay($client, $server, $request);
                    $is_first = false;
                } else {
                    http_relay($client, $server, null);
                }
            }
        } catch(Throwable $e) {
            echo "CLOSE $url ".$e->getMessage()."\n";
            stream_socket_shutdown($server, STREAM_SHUT_RDWR);
            stream_socket_shutdown($client, STREAM_SHUT_RDWR);
        }
    }
}

function start_server(int $port): void {
    $server_sock = stream_socket_server("tcp://0.0.0.0:$port", $errno, $errstr);
    if(!$server_sock) {
        echo "start_server stream_socket_server failed: $errstr\n";
        return;
    }

    echo "proxy server started at $port\n\n";

    while(true) {
        $client_sock = stream_socket_accept($server_sock);
        if(!$client_sock) continue;
        $pid = pcntl_fork();
        if($pid < 0) continue;
        if($pid == 0) {
            start_relay($client_sock);
        }
    }
}

if(!PHP_ZTS) {
    echo "ZTS版のPHPを使用してください。\n";
} else {
    start_server(isset($argv[1]) ? intval($argv[1]) : 8080);
}