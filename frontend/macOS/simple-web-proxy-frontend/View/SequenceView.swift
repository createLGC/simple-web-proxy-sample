//
//  SequenceView.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/06/13.
//

import SwiftUI

struct SequenceView: View {
    var viewConfig: ViewConfigObject
    
    @Binding var filteredEntries: [HAR_entry]
    @Binding var selectedEntryIds: Set<HAR_entry.ID>
    @Binding var lastSelectedEntry: HAR_entry?
    
    let searchField: AnyView
    
    var body: some View {
        VSplitView {
            LogTable(viewColumns: viewConfig.columns, entries: filteredEntries, selectedEntryIds: $selectedEntryIds)
            searchField
            if let entry = lastSelectedEntry {
                EntryView(entry: entry, viewConfig: viewConfig)
            }
        }
    }
}
