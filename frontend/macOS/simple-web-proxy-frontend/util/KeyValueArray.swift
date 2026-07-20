//
//  KeyValueArray.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/08/08.
//

import Foundation

typealias KeyValueArray = [(key: String, value: String)]

extension KeyValueArray {
    
    mutating func append(key: String, value: String) {
        self.append((key: key, value: value))
    }
    
    func get(_ key: String, caseInsensitive: Bool = false) -> String? {
        let whereClosure = caseInsensitive
            ? { (_key: String, _: String) -> Bool in _key.caseInsensitiveCompare(key) == .orderedSame }
            : { (_key: String, _: String) -> Bool in _key == key }
        return self.first(where: whereClosure)?.value
    }
    
    func getAll(_ key: String, caseInsensitive: Bool = false) -> [String] {
        let whereClosure = caseInsensitive
            ? { (_key: String, _: String) -> Bool in _key.caseInsensitiveCompare(key) == .orderedSame }
            : { (_key: String, _: String) -> Bool in _key == key }
        return self.filter(whereClosure).map { $0.value }
    }
    
}
