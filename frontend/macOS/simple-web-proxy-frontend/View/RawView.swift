//
//  RawView.swift
//  PyProxy
//
//  Created by tester on 2025/12/02.
//

import SwiftUI

struct RawView: View {
    let value: String
    
    var body: some View {
        TextEditor(text: .constant(value))
            .font(.system(size: 14, design: .monospaced))
    }
}
