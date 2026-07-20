//
//  CustomRequestView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/03.
//

import SwiftUI
import JavaScriptCore

struct CustomView: View {
    let html: String
    
    init(object: Encodable, script: String, function: String) {
        let context = JSContext()!
        context.evaluateScript(script)
        context.evaluateScript("function createView(json, fn) { const obj = JSON.parse(json);return fn ? fn(obj) : `<pre>${JSON.stringify(obj, null, 4)}</pre>`; };")
        let jsonString = String(data: try! JSONEncoder().encode(object), encoding: .utf8)!
        let createView = context.objectForKeyedSubscript("createView")!
        let fn = context.objectForKeyedSubscript(function)
        let result = createView.call(withArguments: [jsonString, fn ?? JSValue(nullIn: context)!])!
        html = result.toString()
    }
    
    var body: some View {
        HTMLView(html: html, baseURL: nil)
    }
}
