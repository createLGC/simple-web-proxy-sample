//
//  LogTable.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/25.
//

import SwiftUI

struct LogTable: View {
    @Bindable var viewColumns: ViewConfigObject.Columns

    var entries: [HAR_entry]
    @Binding var selectedEntryIds: Set<HAR_entry.ID>
    
    @State var showPopover = false
    
    var body: some View {
        ZStack(alignment: .topTrailing) {
            Table(entries, selection: $selectedEntryIds) {
                if viewColumns.clientIPAddress {
                    TableColumn("Client IP Address") {
                        Text($0._clientIPAddress ?? String(localized: "none"))
                    }
                }
                if viewColumns.start {
                    TableColumn("Start") {
                        Text(formatStartedDateTime(startedDateTime: $0.startedDateTime))
                    }
                }
                if viewColumns.url {
                    TableColumn("URL", value: \.request.url)
                }
                if viewColumns.method {
                    TableColumn("Method", value: \.request.method)
                }
                if viewColumns.requestContentType {
                    TableColumn("Content-Type(Request)") {
                        if let contentType = $0.request.contentType {
                            Text(contentType)
                        } else {
                            Text("no")
                        }
                    }
                }
                if viewColumns.status {
                    TableColumn("Status") {
                        Text(String($0.response.status))
                    }
                }
                if viewColumns.responseContentType {
                    TableColumn("Content-Type(Response)") {
                        if let contentType = $0.response.contentType {
                            Text(contentType)
                        } else {
                            Text("no")
                        }
                    }
                }
            }
            Button {
                showPopover = true
            } label: {
                Image(systemName: "gearshape.fill")
            }
            .buttonStyle(.plain)
            .padding(6)
            .popover(isPresented: $showPopover) {
                VStack(alignment: .leading) {
                    Toggle("Client IP Address", isOn: $viewColumns.clientIPAddress)
                    Toggle("Start", isOn: $viewColumns.start)
                    Toggle("URL", isOn: $viewColumns.url)
                    Toggle("Method", isOn: $viewColumns.method)
                    Toggle("Content-Type(Request)", isOn: $viewColumns.requestContentType)
                    Toggle("Status", isOn: $viewColumns.status)
                    Toggle("Content-Type(Response)", isOn: $viewColumns.responseContentType)
                }
                .padding()
            }
        }
    }
    
    func formatStartedDateTime(startedDateTime: String) -> String {
        if let date = ISO8601DateFormatter().date(from: startedDateTime) {
            let f = DateFormatter()
            f.dateStyle = .none
            f.timeStyle = .medium
            f.locale = Locale(identifier: "ja_JP")
            return f.string(from: date)
        } else {
            return startedDateTime
        }
    }
}
