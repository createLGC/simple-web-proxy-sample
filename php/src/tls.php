<?php

enum TLSContentType: string {
    case invalid = "\x0";
    case change_cipher_spec = "\x14";
    case alert = "\x15";
    case handshake = "\x16";
    case application_data = "\x17";
}

enum TLSProtocolVersion: string {
    case v1_0 = "\x03\x01";
    case v1_1 = "\x03\x02";
    case v1_2 = "\x03\x03";
    case v1_3 = "\x03\x04";
}

class TLSRecord {

    public static function parse(&$stream): TLSRecord {
        $_type = stream_get_contents($stream, 1);
        if($_type === false) throw new Exception("TLSRecord::parse ContentType stream_get_contents failed.");
        try {
            $type = TLSContentType::from($_type);
        } catch(Throwable $e) {
            throw new Exception("TLSRecord::parse TLSContentType::from ".$e->getMessage());
        }

        $_version = stream_get_contents($stream, 2);
        if($_version === false) throw new Exception("TLSRecord::parse ProtocolVersion stream_get_contents failed.");
        try {
            $version = TLSProtocolVersion::from($_version);
        } catch(Throwable $e) {
            throw new Exception("TLSRecord::parse TLSProtocolVersion::from ".$e->getMessage());
        }

        $_length_bytes = stream_get_contents($stream, 2);
        if($_length_bytes === false) throw new Exception("TLSRecord::parse length stream_get_contents failed.");

        $_length = unpack("n", $_length_bytes);
        if($_length === false) throw new Exception("TLSRecord::parse length unpack failed.");
        $length = $_length[1];

        $fragment = stream_get_contents($stream, $length);
        if($fragment === false) throw new Exception("TLSRecord::parse fragment stream_get_contents failed.");

        return new TLSRecord($type, $version, $length, $fragment);
    }

    public TLSContentType $type;
    public TLSProtocolVersion $version;
    public int $length; //2バイト
    public string $fragment;
        
    public function __construct(TLSContentType $type, TLSProtocolVersion $version, int $length, string $fragment) {
        $this->type = $type;
        $this->version = $version;
        $this->length = $length;
        $this->fragment = $fragment;
    }

    public function write(&$stream): void {
        if(
            stream_socket_sendto($stream, $this->type->value) === false ||
            stream_socket_sendto($stream, $this->version->value) === false ||
            stream_socket_sendto($stream, pack("n", $this->length)) === false ||
            stream_socket_sendto($stream, $this->fragment) === false
        ) {
            throw new Exception("TLSRecord write stream_socket_sendto failed.");
        }
    }

}