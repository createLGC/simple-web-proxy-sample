//
//  NSTextView+extension.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/07/05.
//

import Foundation
import AppKit

extension NSTextView {
    open override var frame: CGRect {
        didSet {
            self.isAutomaticQuoteSubstitutionEnabled = false
        }
    }
}
