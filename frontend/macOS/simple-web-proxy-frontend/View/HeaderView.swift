//
//  HeaderView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/01.
//

import SwiftUI

struct HeaderView: View {
    let headers: [HAR_header]
    
    @State var selectedHeaderIds: Set<HAR_header.ID> = []
   
    var body: some View {
        Table(headers, selection: $selectedHeaderIds) {
            TableColumn("Key") {
                Text($0.name)
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
            TableColumn("Value") {
                Text($0.value)
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
        }
    }
}
