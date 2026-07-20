def judge_decrypt(client: str, remote: str) -> bool:
    return True
        
def get_external_proxy(client: str, remote: str) -> tuple[str, int]:
    return ("localhost", 8888)
        
def modify_request(host: str, request_data):
    return request_data
        
def modify_response(host: str, request_data, response_data):
    return response_data