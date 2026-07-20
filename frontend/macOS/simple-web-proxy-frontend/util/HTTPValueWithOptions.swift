//
//  ValueWithOptions.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/08/08.
//

import Foundation

class HTTPValueWithOptions {
    var value: String
    var options: KeyValueArray
    
    init(string: String) {
        let components = string.split(separator: ";").map { String($0.trimmingCharacters(in: .whitespaces)) }
        value = components[0]
        options = components.dropFirst().map {
            let optionsComponents = $0.split(separator: "=").map { String($0) }
            return (key: optionsComponents[0], value: optionsComponents[1])
        }
    }
}
