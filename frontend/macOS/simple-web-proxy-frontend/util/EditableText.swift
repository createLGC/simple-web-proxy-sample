//
//  EditableText.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/07/12.
//

import Foundation
import SwiftUI

struct EditableText: View {
    @Binding var text: String
    let defaultText: String
    
    @State var tmpText = ""
    
    @State var isEditing = false
    
    var body: some View {
        if isEditing {
            TextField("", text: $tmpText)
                .onAppear {
                    tmpText = text
                }
                .onSubmit {
                    text = tmpText.isEmpty ? defaultText : tmpText
                    isEditing = false
                }
        } else {
            Text(text)
                .onTapGesture(count: 2) {
                    isEditing = true
                }
        }
    }
}
