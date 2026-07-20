//
//  WindowController.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/11/27.
//

import Foundation
import AppKit
import SwiftUI

class WindowController<RootView: View>: NSWindowController {
    convenience init(title: String? = nil, rootView: RootView) {
        let hostingController = NSHostingController(rootView: rootView)
        let window = NSWindow(contentViewController: hostingController)
        if let title = title {
            window.title = title
        }
        window.setContentSize(NSSize(width: 600, height: 800))
        self.init(window: window)
    }
}
