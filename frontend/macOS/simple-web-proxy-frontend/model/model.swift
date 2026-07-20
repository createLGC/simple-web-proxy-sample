//
//  model.swift
//  PyProxy
//
//  Created by tester on 2025/11/29.
//

import SwiftData

public func getSchemaTypes() -> [any PersistentModel.Type] {
    [
        SwitchableField.self,
        SwitchableFieldGroup.self,
        ServerConfigObject.self,
        ViewConfigObject.Columns.self,
        ViewConfigObject.self
    ]
}
