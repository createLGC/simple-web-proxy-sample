//
//  StructureView.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/06/13.
//

import AppKit
import SwiftUI

struct StructureView: View {
    var viewConfig: ViewConfigObject
    
    @ObservedObject var structure: Structure
    @Binding var selectedEntryIds: Set<HAR_entry.ID>
    @Binding var lastSelectedEntry: HAR_entry?
    
    let searchField: AnyView
    
    var body: some View {
        HSplitView {
            VStack(spacing: 0) {
                List(selection: $selectedEntryIds) {
                    ForEach(structure.hosts, id: \.name) { host in
                        Section {
                            ForEach(host.entries, id: \.id) { entry in
                                let entryLabel = entry.request.pathAndQuery
                                Text(entryLabel.isEmpty ? String(localized: "unknown") : entryLabel)
                                    .tag(entry.id)
                                    .contextMenu {
                                        Button {
                                            NSPasteboard.general.clearContents()
                                            NSPasteboard.general.setString(entry.request.url, forType: .string)
                                        } label: {
                                            Text("copy_url")
                                        }
                                    }
                            }
                        } header: {
                            Text(host.name)
                                .font(.headline)
                                .foregroundColor(Color(.labelColor))
                        }
                    }
                }
                .listStyle(.sidebar)
                .scrollContentBackground(.hidden)
                .background(Color(.textBackgroundColor))
                searchField
            }
            .frame(minWidth: 200)
            if let entry = lastSelectedEntry {
                EntryView(entry: entry, viewConfig: viewConfig)
            }
        }
    }
}
