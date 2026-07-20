from __future__ import annotations
from functools import reduce
from enum import IntEnum
from dataclasses import dataclass
from ssl import TLSVersion

@dataclass
class Extension:
    class Type(IntEnum):
        server_name = 0,
        max_fragment_length = 1,
        client_certificate_url = 2,
        trusted_ca_keys = 3,
        truncated_hmac = 4,
        status_request = 5,
        user_mapping = 6,
        client_authz = 7,
        server_authz = 8,
        cert_type = 9,
        supported_groups = 10,
        ec_point_formats = 11,
        srp = 12,
        signature_algorithms = 13,
        use_srtp = 14,
        heart_beat = 15,
        application_layer_protocol_negotiation = 16,
        status_request_v2 = 17,
        signed_certificate_timestamp = 18,
        client_certificate_type = 19,
        server_certificate_type = 20,
        padding = 21,
        encrypt_then_mac = 22,
        extended_master_secret = 23,
        token_binding = 24,
        cached_info = 25,
        tls_lts = 26,
        compress_certificate = 27,
        record_size_limit = 28,
        pwd_protect = 29,
        pwd_clear = 30,
        password_salt = 31,
        ticket_pinning = 32,
        tls_cert_with_extern_psk = 33,
        delegated_credential = 34,
        session_ticket = 35,
        TLMSP = 36,
        TLMSP_proxying = 37,
        TLMSP_delegate = 38,
        supported_ekt_ciphers = 39,
        ## RESERVED = 40,
        pre_shared_key = 41,
        early_data = 42,
        supported_versions = 43,
        cookie = 44,
        psk_key_exchange_modes = 45,
        ## RESERVED = 46,
        certificate_authorities = 47,
        oid_filters = 48,
        post_handshake_auth = 49,
        signature_algorithms_cert = 50,
        key_share = 51,
        transparency_info = 52,
        connection_id_deprecated = 53,
        connection_id = 54,
        external_id_hash = 55,
        external_session_id = 56,
        quic_transport_parameters = 57,
        ticket_request = 58,
        dnssec_chain = 59,
        sequence_number_encryption_algorithms = 60,
        rrc = 61,
        ech_outer_extensions = 64768,
        encrypted_client_hello = 65037,
        renegotiation_info = 65281

    type: Extension.Type
    data: bytes

    @classmethod
    def from_bytes(cls, data: bytes) -> Extension:
        i = 0
        type = Extension.Type(int.from_bytes(data[i:i+2], byteorder='big'))
        i += 2
        length = int.from_bytes(data[i:i+2], byteorder='big')
        i += 2
        return Extension(type, data[i:i+length])
    
    def to_bytes(self) -> bytes:
        return (
            self.type.value.to_bytes(2, byteorder='big') +
            len(self.data).to_bytes(2, byteorder='big') +
            self.data
        )

@dataclass
class ClientHello:
    version: TLSVersion
    random: bytes
    session_id: bytes
    cipher_suites: bytes
    compression_methods: bytes
    extensions: list[Extension]

    @classmethod
    def from_bytes(cls, data: bytes) -> ClientHello:
        i = 0
        version = TLSVersion(int.from_bytes(data[i:i+2], byteorder='big'))
        i += 2
        random = data[i:i+32]
        i += 32
        session_id_length = int(data[i])
        i += 1
        session_id = data[i:i+session_id_length]
        i += session_id_length
        cipher_suites_length = int.from_bytes(data[i:i+2], byteorder='big')
        i += 2
        cipher_suites = data[i:i+cipher_suites_length]
        i += cipher_suites_length
        compression_methods_length = int(data[i])
        i += 1
        compression_methods = data[i:i+compression_methods_length]
        i += compression_methods_length
        extensions_length = int.from_bytes(data[i:i+2], byteorder='big')
        i += 2
        extensions = []
        while i < len(data):
            extension = Extension.from_bytes(data[i:])
            extensions.append(extension)
            i += len(extension.data) + 4
        return ClientHello(version, random, session_id, cipher_suites, compression_methods, extensions)
    
    def to_bytes(self) -> bytes:
        extensions_bytes = reduce(lambda acc, e: acc + e.to_bytes(), self.extensions, b'')
        return (
            self.version.value.to_bytes(2, byteorder='big') +
            self.random +
            len(self.session_id).to_bytes(1) +
            self.session_id +
            len(self.cipher_suites).to_bytes(2, byteorder='big') +
            self.cipher_suites +
            len(self.compression_methods).to_bytes(1) +
            self.compression_methods +
            len(extensions_bytes).to_bytes(2, byteorder='big') +
            extensions_bytes
        )
    
    @property
    def alpn_protocols(self) -> list[str] | None:
        for e in self.extensions:
            if e.type == Extension.Type.application_layer_protocol_negotiation:
                protos_data = e.data[2:]
                protos = []
                while len(protos_data) > 0:
                    length = int(protos_data[0])
                    protos.append(protos_data[1:1+length].decode())
                    protos_data = protos_data[1+length:]
                return protos

@dataclass
class ServerHello:
    version: TLSVersion
    random: bytes
    session_id: bytes
    cipher_suite: int
    compression_method: int
    extensions: list[Extension]

    @classmethod
    def from_bytes(cls, data: bytes) -> ServerHello:
        i = 0
        version = TLSVersion(int.from_bytes(data[i:i+2], byteorder='big'))
        i += 2
        random = data[i:i+32]
        i += 32
        session_id_length = int(data[i])
        i += 1
        session_id = data[i:i+session_id_length]
        i += session_id_length
        cipher_suite = int.from_bytes(data[i:i+2], byteorder='big')
        i += 2
        compression_method = int(data[i])
        i += 1
        extensions_length = int.from_bytes(data[i:i+2], byteorder='big')
        i += 2
        extensions = []
        while i < len(data):
            extension = Extension.from_bytes(data[i:])
            extensions.append(extension)
            i += len(extension.data) + 4
        return ServerHello(version, random, session_id, cipher_suite, compression_method, extensions)
    
    def to_bytes(self) -> bytes:
        extensions_bytes = reduce(lambda acc, e: acc + e.to_bytes(), self.extensions, b'')
        return (
            self.version.value.to_bytes(2, byteorder='big') +
            self.random +
            len(self.session_id).to_bytes(1) +
            self.session_id +
            self.cipher_suite.to_bytes(2, byteorder='big') +
            self.compression_method.to_bytes(1) +
            len(extensions_bytes).to_bytes(2, byteorder='big') +
            extensions_bytes
        )
    
    @property
    def alpn_protocol(self) -> str:
        for e in self.extensions:
            if e.type == Extension.Type.application_layer_protocol_negotiation:
                return e.data[1:].encode()