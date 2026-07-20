//
//  ConfigObject.swift
//  Flex Proxy
//
//  Created by tester on 2023/11/26.
//

import Foundation
import SwiftData

@Model
public final class ServerConfigObject: Codable {
    public static func new(scriptURL: URL) -> ServerConfigObject {
        let script = """
        def judge_decrypt(client: str, remote: str) -> bool:
            return True
        
        def get_external_proxy(client: str, remote: str) -> tuple[str, int]:
            return (\"localhost\", 8888)
        
        def modify_request(host: str, request_data):
            return request_data
        
        def modify_response(host: str, request_data, response_data):
            return response_data
        
        """
        try! script.write(to: scriptURL, atomically: true, encoding: .utf8)
        return ServerConfigObject(
            acl: SwitchableFieldGroup(
                on: nil,
                fields: []
            ),
            sslProxying: SwitchableFieldGroup(
                on: true,
                fields: [
                    SwitchableField(
                        on: true,
                        value: "judge_decrypt"
                    )
                ]
            ),
            externalProxy: SwitchableFieldGroup(
                on: false,
                fields: [
                    SwitchableField(
                        on: true,
                        value: "get_external_proxy"
                    )
                ]
            ),
            modifyRequest: SwitchableFieldGroup(
                on: false,
                fields: [
                    SwitchableField(
                        on: true,
                        value: "modify_request"
                    )
                ]
            ),
            modifyResponse: SwitchableFieldGroup(
                on: false,
                fields: [
                    SwitchableField(
                        on: true,
                        value: "modify_response"
                    )
                ]
            ),
            scriptPath: scriptURL.path(percentEncoded: false)
        )
    }
    
    @Relationship(deleteRule: .cascade) var acl: SwitchableFieldGroup
    @Relationship(deleteRule: .cascade) var sslProxying: SwitchableFieldGroup
    @Relationship(deleteRule: .cascade) var externalProxy: SwitchableFieldGroup
    @Relationship(deleteRule: .cascade) var modifyRequest: SwitchableFieldGroup
    @Relationship(deleteRule: .cascade) var modifyResponse: SwitchableFieldGroup
    var scriptPath: String
    
    init(acl: SwitchableFieldGroup, sslProxying: SwitchableFieldGroup, externalProxy: SwitchableFieldGroup, modifyRequest: SwitchableFieldGroup, modifyResponse: SwitchableFieldGroup, scriptPath: String) {
        self.acl = acl
        self.sslProxying = sslProxying
        self.externalProxy = externalProxy
        self.modifyRequest = modifyRequest
        self.modifyResponse = modifyResponse
        self.scriptPath = scriptPath
    }
    
    public func isAllowedAddress(address: String) -> Bool {
        acl.fields.contains(where: { $0.on && $0.value == address })
    }
    
    public func allowAddress(address: String) {
        if let index = acl.fields.firstIndex(where: { $0.value == address }) {
            acl.fields[index].on = true
        } else {
            acl.fields.append(SwitchableField(on: true, value: address))
        }
    }
    
    enum CodingKeys: CodingKey {
        case acl
        case sslProxying
        case externalProxy
        case modifyRequest
        case modifyResponse
        case scriptPath
    }
    
    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        acl = try container.decode(SwitchableFieldGroup.self, forKey: .acl)
        sslProxying = try container.decode(SwitchableFieldGroup.self, forKey: .sslProxying)
        externalProxy = try container.decode(SwitchableFieldGroup.self, forKey: .externalProxy)
        modifyRequest = try container.decode(SwitchableFieldGroup.self, forKey: .modifyRequest)
        modifyResponse = try container.decode(SwitchableFieldGroup.self, forKey: .modifyResponse)
        scriptPath = try container.decode(String.self, forKey: .scriptPath)
    }
        
    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(acl, forKey: .acl)
        try container.encode(sslProxying, forKey: .sslProxying)
        try container.encode(externalProxy, forKey: .externalProxy)
        try container.encode(modifyRequest, forKey: .modifyRequest)
        try container.encode(modifyResponse, forKey: .modifyResponse)
        try container.encode(scriptPath, forKey: .scriptPath)
    }
    
    public static func load(from url: URL) throws -> Self {
        try JSONDecoder().decode(Self.self, from: try Data(contentsOf: url))
    }
    
    public func save(to url: URL) throws {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        try encoder.encode(self).write(to: url)
    }
}
