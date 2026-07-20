//
//  ViewConfigObject.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/25.
//

import Foundation
import SwiftData

@Model
public final class ViewConfigObject: Codable {
    @Model
    final class Columns: Codable {
        static func new() -> Columns {
            return Columns(clientIPAddress: true, start: true, url: true, method: true, requestContentType: true, status: true, responseContentType: true)
        }
        
        var clientIPAddress: Bool
        var start: Bool
        var url: Bool
        var method: Bool
        var requestContentType: Bool
        var status: Bool
        var responseContentType: Bool
        
        init(clientIPAddress: Bool, start: Bool, url: Bool, method: Bool, requestContentType: Bool, status: Bool, responseContentType: Bool) {
            self.clientIPAddress = clientIPAddress
            self.start = start
            self.url = url
            self.method = method
            self.requestContentType = requestContentType
            self.status = status
            self.responseContentType = responseContentType
        }
        
        enum CodingKeys: CodingKey {
            case clientIPAddress
            case start
            case url
            case method
            case requestContentType
            case status
            case responseContentType
        }
        
        init(from decoder: Decoder) throws {
            let container = try decoder.container(keyedBy: CodingKeys.self)
            clientIPAddress = try container.decode(Bool.self, forKey: .clientIPAddress)
            start = try container.decode(Bool.self, forKey: .start)
            url = try container.decode(Bool.self, forKey: .url)
            method = try container.decode(Bool.self, forKey: .method)
            requestContentType = try container.decode(Bool.self, forKey: .requestContentType)
            status = try container.decode(Bool.self, forKey: .status)
            responseContentType = try container.decode(Bool.self, forKey: .responseContentType)
        }
            
        func encode(to encoder: Encoder) throws {
            var container = encoder.container(keyedBy: CodingKeys.self)
            try container.encode(clientIPAddress, forKey: .clientIPAddress)
            try container.encode(start, forKey: .start)
            try container.encode(url, forKey: .url)
            try container.encode(method, forKey: .method)
            try container.encode(requestContentType, forKey: .requestContentType)
            try container.encode(status, forKey: .status)
            try container.encode(responseContentType, forKey: .responseContentType)
        }
    }

    public static func new() -> ViewConfigObject {
        return ViewConfigObject(
            columns: Columns.new(),
            customRequestView: SwitchableFieldGroup(
                on: true,
                fields: [
                    SwitchableField(on: true, value: "createRequestView")
                ]
            ),
            customResponseView: SwitchableFieldGroup(
                on: true,
                fields: [
                    SwitchableField(on: true, value: "createResponseView")
                ]
            ),
            script: """
            function createRequestView(request) {
                return `
                    <p>${request.url}</p>
                `
            }
            
            function createResponseView(response) {
                return `
                    <p>${response.status} ${response.statusText}</p>
                `
            }
            """
        )
    }
    
    @Relationship(deleteRule: .cascade) var columns: Columns
    @Relationship(deleteRule: .cascade) var customRequestView: SwitchableFieldGroup
    @Relationship(deleteRule: .cascade) var customResponseView: SwitchableFieldGroup
    var script: String
    
    init(columns: Columns, customRequestView: SwitchableFieldGroup, customResponseView: SwitchableFieldGroup, script: String) {
        self.columns = columns
        self.customRequestView = customRequestView
        self.customResponseView = customResponseView
        self.script = script
    }
    
    enum CodingKeys: CodingKey {
        case columns
        case customRequestView
        case customResponseView
        case script
    }
    
    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        columns = try container.decode(Columns.self, forKey: .columns)
        customRequestView = try container.decode(SwitchableFieldGroup.self, forKey: .customRequestView)
        customResponseView = try container.decode(SwitchableFieldGroup.self, forKey: .customResponseView)
        script = try container.decode(String.self, forKey: .script)
    }
        
    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(columns, forKey: .columns)
        try container.encode(customRequestView, forKey: .customRequestView)
        try container.encode(customResponseView, forKey: .customResponseView)
        try container.encode(script, forKey: .script)
    }
}
