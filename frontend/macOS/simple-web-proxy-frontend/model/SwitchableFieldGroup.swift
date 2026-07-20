//
//  ScriptGroup.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/06/28.
//

import Foundation
import SwiftData

@Model
final class SwitchableFieldGroup: Codable {
    var on: Bool?
    @Relationship(deleteRule: .cascade, inverse: \SwitchableField.group) var fields: [SwitchableField]
    
    var validFieldValue: String? {
        on == false ? nil : fields.first(where: { $0.on })?.value
    }
    
    init(on: Bool?, fields: [SwitchableField]) {
        self.on = on
        self.fields = fields
    }
    
    enum CodingKeys: CodingKey {
        case on
        case fields
    }
    
    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        if let fields = try? container.decode([SwitchableField].self) {
            on = nil
            self.fields = fields
        } else {
            struct _Group: Codable {
                let on: Bool
                let fields: [SwitchableField]
            }
            let nested = try container.decode(_Group.self)
            on = nested.on
            fields = nested.fields
        }
    }
    
    func encode(to encoder: Encoder) throws {
        if on == nil {
            var container = encoder.unkeyedContainer()
            for field in fields {
                try container.encode(field)
            }
        } else {
            var container = encoder.container(keyedBy: CodingKeys.self)
            try container.encode(on, forKey: .on)
            try container.encode(fields, forKey: .fields)
        }
    }
}
