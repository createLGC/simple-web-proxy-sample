//
//  WebView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/01.
//

import Foundation
import SwiftUI
import WebKit

struct HTMLView: NSViewRepresentable {
    typealias NSViewType = WKWebView
    
    let html: String
    let baseURL: URL?
    
    func makeNSView(context: Context) -> WKWebView {
        let webView = WKWebView()
        webView.loadHTMLString(html, baseURL: baseURL)
        return webView
    }
    
    func updateNSView(_ nsView: WKWebView, context: Context) {
        nsView.loadHTMLString(html, baseURL: baseURL)
    }
}
