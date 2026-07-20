import datetime, secrets
from tempfile import NamedTemporaryFile, TemporaryDirectory
from typing import Optional
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives.asymmetric.types import PrivateKeyTypes
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.serialization import pkcs12
from cryptography import x509
from cryptography.x509.oid import NameOID

class KeyCert:
    def __init__(self, keypath: str, certpath: str, password: bytes) -> None:
        self.keypath = keypath
        self.certpath = certpath
        self.password = password

def create_ca(p12_info: Optional[tuple[bytes, bytes]] = None) -> tuple[PrivateKeyTypes, x509.Certificate]:
    if p12_info:
        p12_data, p12_password = p12_info
        ca_private_key, ca_cert, additional_certificates = pkcs12.load_key_and_certificates(p12_data, p12_password)
    else:
        ca_private_key = rsa.generate_private_key(public_exponent=65537, key_size=4096)
        subject = issuer = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "Proxy CA")])

        now = datetime.datetime.now(datetime.timezone.utc)
        ca_cert = (
            x509.CertificateBuilder()
                .subject_name(subject)
                .issuer_name(issuer)
                .public_key(ca_private_key.public_key())
                .serial_number(x509.random_serial_number())
                .not_valid_before(now)
                .not_valid_after(now + datetime.timedelta(days=365))
                .add_extension(x509.BasicConstraints(ca=True, path_length=None), critical=True)
                .add_extension(x509.SubjectKeyIdentifier.from_public_key(ca_private_key.public_key()), critical=False)
                .sign(ca_private_key, hashes.SHA256())
        )
        with open('./CA.pem', 'wb') as ca_cert_file:
            ca_cert_file.write(
                ca_cert.public_bytes(serialization.Encoding.PEM)
            )
    return ca_private_key, ca_cert

def create_server(dir: str, ca_private_key: PrivateKeyTypes, ca_cert: x509.Certificate, host: str) -> KeyCert:
    server_private_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    server_subject = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, host)])
    csr = (
        x509.CertificateSigningRequestBuilder()
           .subject_name(server_subject)
           .add_extension(x509.SubjectAlternativeName([x509.DNSName(host)]), critical=False)
           .sign(server_private_key, hashes.SHA256())
    )
    now = datetime.datetime.now(datetime.timezone.utc)
    server_cert = (
        x509.CertificateBuilder()
            .subject_name(csr.subject)
            .issuer_name(ca_cert.subject)
            .public_key(csr.public_key())
            .serial_number(x509.random_serial_number())
            .not_valid_before(now)
            .not_valid_after(now + datetime.timedelta(days=365))
    )
    for ext in csr.extensions:
        server_cert = server_cert.add_extension(ext.value, ext.critical)
    server_cert = (
        server_cert
            .add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
            .add_extension(x509.ExtendedKeyUsage((x509.oid.ExtendedKeyUsageOID.SERVER_AUTH,)), critical=False)
            .add_extension(x509.AuthorityKeyIdentifier.from_issuer_public_key(ca_private_key.public_key()), critical=False)
            .sign(ca_private_key, hashes.SHA256())
    )
    server_password = secrets.token_bytes(32)
    with NamedTemporaryFile(dir=dir, delete=False) as server_key_file:
        server_key_file.write(
            server_private_key.private_bytes(
                encoding=serialization.Encoding.PEM,
                format=serialization.PrivateFormat.TraditionalOpenSSL,
                encryption_algorithm=serialization.BestAvailableEncryption(server_password)
            )
        )
        with NamedTemporaryFile(dir=dir, delete=False) as server_cert_file:
            server_cert_file.write(
                server_cert.public_bytes(serialization.Encoding.PEM)
            )
            return KeyCert(server_key_file.name, server_cert_file.name, server_password)

def setup(p12_info: Optional[tuple[bytes, bytes]] = None) -> None:
    dir = TemporaryDirectory()
    ca_private_key, ca_cert = create_ca(p12_info)
    servers: dict[str, KeyCert] = {}
    
    def _get(host: str) -> tuple[str, str, bytes]:
        if host not in servers:
            servers[host] = create_server(dir.name, ca_private_key, ca_cert, host)
        server = servers[host]
        return server.certpath, server.keypath, server.password
    
    global get
    get = _get