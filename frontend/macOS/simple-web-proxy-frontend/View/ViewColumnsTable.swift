//
//  ViewColumnsTable.swift
//  PyProxy
//
//  Created by tester on 2025/11/29.
//

import SwiftUI

struct ViewColumnsTable: View {
    class ColumnObject: Identifiable {
        var id: String { title }
        let title: String
        let isOn: Binding<Bool>
        
        init(_ title: String, isOn: Binding<Bool>) {
            self.title = title
            self.isOn = isOn
        }
    }
    
    @Bindable var viewColumns: ViewConfigObject.Columns
    
    var body: some View {
        VStack {
            Text("edit_the_columns_of_sequence")
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(.bottom, 1)
            Table(of: ColumnObject.self) {
                TableColumn(Text("valid")) {
                    Toggle("", isOn: $0.isOn)
                }
                .width(25)
                TableColumn(Text("column_name"), value: \.title)
            } rows: {
                TableRow(ColumnObject("Client IP Address", isOn: $viewColumns.clientIPAddress))
                TableRow(ColumnObject("Start", isOn: $viewColumns.start))
                TableRow(ColumnObject("URL", isOn: $viewColumns.url))
                TableRow(ColumnObject("Method", isOn: $viewColumns.method))
                TableRow(ColumnObject("Content-Type(Request)", isOn: $viewColumns.requestContentType))
                TableRow(ColumnObject("Status", isOn: $viewColumns.status))
                TableRow(ColumnObject("Content-Type(Response)", isOn: $viewColumns.responseContentType))
            }
        }
    }
}
