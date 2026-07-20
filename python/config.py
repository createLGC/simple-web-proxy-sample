from __future__ import annotations
import importlib.util, json, re
from dataclasses import dataclass
from collections.abc import Callable
from typing import Optional
from http1 import HTTP1Request, HTTP1Response

@dataclass
class SwitchableField:
    on: bool
    value: str

    @classmethod
    def from_json(cls, json_dict: dict) -> SwitchableField:
        return SwitchableField(json_dict['on'], json_dict['value'])

@dataclass
class SwitchableFieldGroup:
    on: bool
    fields: list[SwitchableField]

    @classmethod
    def from_json(cls, json_dict: dict) -> SwitchableFieldGroup:
        return SwitchableFieldGroup(json_dict['on'], [SwitchableField.from_json(field) for field in json_dict['fields']])
    
    @property
    def valid_field_value(self) -> Optional[str]:
        if self.on:
            for field in self.fields:
                if field.on:
                    return field.value

@dataclass
class ConfigObject:
    acl: list[SwitchableField]
    sslProxying: SwitchableFieldGroup
    externalProxy: SwitchableFieldGroup
    modifyRequest: SwitchableFieldGroup
    modifyResponse: SwitchableFieldGroup
    scriptPath: str

    @classmethod
    def from_json(cls, json_dict: dict) -> ConfigObject:
        return ConfigObject(
            acl=[SwitchableField.from_json(address) for address in json_dict['acl']],
            sslProxying=SwitchableFieldGroup.from_json(json_dict['sslProxying']),
            externalProxy=SwitchableFieldGroup.from_json(json_dict['externalProxy']),
            modifyRequest=SwitchableFieldGroup.from_json(json_dict['modifyRequest']),
            modifyResponse=SwitchableFieldGroup.from_json(json_dict['modifyResponse']),
            scriptPath=json_dict['scriptPath']
        )
    
    def __post_init__(self):
        spec = importlib.util.spec_from_file_location("script", self.scriptPath)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        self.script = module

    def judgeAcceptable(self, client_address: str) -> bool:
        for address in self.acl:
            if address.on:
                pattern = re.compile(address.value)
                match = pattern.fullmatch(client_address)
                if match is not None:
                    return True
        return False
    
    @property
    def judgeSslProxying(self) -> Optional[Callable[[str, str], bool]]:
        if valid_script_name := self.sslProxying.valid_field_value:
            return getattr(self.script, valid_script_name, None)
    
    @property
    def getExternalProxy(self) -> Optional[Callable[[str, str], tuple[str, int]]]:
        if valid_script_name := self.externalProxy.valid_field_value:
            return getattr(self.script, valid_script_name, None)
        
    @property
    def requestModifier(self) -> Optional[Callable[[str, HTTP1Request], HTTP1Request]]:
        if valid_script_name := self.modifyRequest.valid_field_value:
            return getattr(self.script, valid_script_name, None)
    
    @property
    def responseModifier(self) -> Optional[Callable[[str, HTTP1Request, HTTP1Response], HTTP1Response]]:
        if valid_script_name := self.modifyResponse.valid_field_value:
            return getattr(self.script, valid_script_name, None)

load = lambda: None

def setup(path: str) -> None:
    global load
    def _load():
        with open(path, 'r') as f:
            return ConfigObject.from_json(json.load(f))
    load = _load
