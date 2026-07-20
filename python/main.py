import asyncio, os, re, signal, sys
from argparse import ArgumentParser, ArgumentTypeError
from getpass import getpass
import cert, config, ipc, server

class MyArgumentParser(ArgumentParser):
    ADDRESS_PATTERN = re.compile(r"^(?P<host>[\w-]+(?:\.[\w-]+)*):(?P<port>\d+)$")

    def __init__(self):
        super().__init__()
        self.add_argument('-a', '--address', type=self.check_address, required=True)
        self.add_argument('-ia', '--ipc-address', type=self.check_address)
        self.add_argument('-c', '--config-file', type=self.check_file)
        self.add_argument('-p12', '--pkcs12-file', type=self.check_file)
        self.add_argument('-pr', '--permission-request', action='store_true')
    
    def check_address(self, address: str) -> tuple[str, int]:
        if match := self.ADDRESS_PATTERN.fullmatch(address):
            return match.group('host'), int(match.group('port'))
        raise ArgumentTypeError(f"Invalid form of address: \"{address}\".\nPlease specify an address in this form: \"host:port\"(e.g. localhost:8888).")
    
    def check_file(self, path: str) -> str:
        if os.path.isfile(path):
            return path
        raise ArgumentTypeError(f"Invalid file path: {path}")

def main():
    args = MyArgumentParser().parse_args()

    if ipc_address := args.ipc_address:
        ipc.setup(ipc_address)
    
    if config_file := args.config_file:
        config.setup(config_file)
    
        if pkcs12_file := args.pkcs12_file:
            with open(pkcs12_file, 'rb') as f:
                p12_data = f.read()
                p12_password = getpass("Please enter the password for the pkcs12 file > ").encode()
                cert.setup((p12_data, p12_password))
        else:
            cert.setup()
    
    server.setup(args.permission_request)

    try:
        asyncio.run(server.start_server(args.address))
    except KeyboardInterrupt:
        pass

if __name__ == "__main__":
    main()
