//
//  QueryStringView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/01.
//

import SwiftUI

struct QueryStringView: View {
    let queryString: [HAR_queryString_item]
    
    @State var selectedItemIds: Set<HAR_queryString_item.ID> = []
    
    var body: some View {
        Table(queryString, selection: $selectedItemIds) {
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
