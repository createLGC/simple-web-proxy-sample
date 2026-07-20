//
//  SwitchableField.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/06/28.
//

import Foundation
import SwiftData

@Model
final class SwitchableField: Identifiable, Codable {
    @Transient public let id = UUID()
    var group: SwitchableFieldGroup?
    var on: Bool
    var value: String
    
    init(on: Bool, value: String) {
        self.on = on
        self.value = value
    }
    
    enum CodingKeys: CodingKey {
        case on
        case value
    }
    
    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        on = try container.decode(Bool.self, forKey: .on)
        value = try container.decode(String.self, forKey: .value)
    }
        
    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(on, forKey: .on)
        try container.encode(value, forKey: .value)
    }
}
