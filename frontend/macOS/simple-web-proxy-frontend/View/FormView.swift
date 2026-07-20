//
//  FormView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/24.
//

import SwiftUI

struct FormView: View {
    let params: [HAR_postData_param]
    
    @State var selectedParamId: HAR_postData_param.ID? = nil
    
    var body: some View {
        Table(params, selection: $selectedParamId) {
            TableColumn("Key") {
                Text($0.name)
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
            TableColumn("Value") {
                Text($0.value ?? String(localized: "none"))
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
        }
    }
}
