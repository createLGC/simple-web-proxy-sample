//
//  TextView.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/07/29.
//

import SwiftUI

struct TextView: View {
    let text: String
    let base64Encoded: Bool
    let filename: String
    
    @State var exporterPresented = false
    
    @State var searchWord = ""
    
    var body: some View {
        ZStack(alignment: .topTrailing) {
            TextEditor(text: .constant(text))
                .font(.system(size: 14, design: .monospaced))
                .searchable(text: $searchWord)
            Button {
                let panel = NSSavePanel()
                panel.nameFieldStringValue = filename
                guard let window = NSApp.keyWindow else { return }
                panel.beginSheetModal(for: window) { response in
                    switch response {
                    case .OK:
                        guard let url = panel.url else { return }
                        let fileData = base64Encoded ? Data(base64Encoded: text) : text.data(using: .utf8)
                        try? fileData?.write(to: url)
                    default:
                        break
                    }
                }
            } label: {
                Image(systemName: "arrow.down.to.line.compact")
                    .padding(4)
                    .contentShape(Rectangle())
            }
            .buttonStyle(.plain)
        }
    }
}
