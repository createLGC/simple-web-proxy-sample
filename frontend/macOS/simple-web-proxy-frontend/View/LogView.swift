//
//  ProjectView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/11/27.
//

import SwiftUI
import Combine
import UniformTypeIdentifiers

class Host: ObservableObject {
    let name: String
    @Published var entries: [HAR_entry]
    
    init(name: String, entries: [HAR_entry]) {
        self.name = name
        self.entries = entries
    }
}

class Structure: ObservableObject {
    @Published var hosts: [Host]
    
    init(_ hosts: [Host] = []) {
        self.hosts = hosts
    }
    
    func addEntry(_ entry: HAR_entry) {
        guard let hostName = entry.host else { return }
        if let host = hosts.first(where: { $0.name == hostName }) {
            host.entries.append(entry)
        } else {
            hosts.append(Host(name: hostName, entries: [entry]))
        }
    }
}

public enum SearchFilter: Equatable {
    case clientIPAddress(String)
    case normal(String)
    
    func match(entry: HAR_entry) -> Bool {
        switch(self) {
        case .clientIPAddress(let ipAddress):
            return entry._clientIPAddress == ipAddress
        case .normal(let word):
            return entry._clientIPAddress?.contains(word) ?? true || entry.request.string.contains(word) || entry.response.string.contains(word)
        }
    }
}

struct LogView: View {
    let server: Server
    
    let id = UUID()
    
    @State var exporterPresented = false
    
    @State private var selectedEntryIds: Set<HAR_entry.ID> = []
    var lastSelectedEntry: Binding<HAR_entry?> {
        Binding(get: { filteredEntries.last { selectedEntryIds.contains($0.id) } }, set: { _ in })
    }
    
    @State var searchInputValue = ""
    @State var searchFilters: [SearchFilter] = []
    
    @State var isSearching = false
    
    @State var entries: [HAR_entry] = []
    
    @State var structure: Structure = Structure()
    
    @State var filteredEntries: [HAR_entry] = []
    
    var searchField: some View {
        HStack(spacing: 0) {
            TextField("Filter", text: $searchInputValue) {
                searchFilters = searchInputValue.components(separatedBy: [" ", "　"]).filter { !$0.isEmpty }.map { .normal($0) }
            }
            .frame(maxWidth: .infinity)
            .disabled(isSearching)
            if isSearching {
                ProgressView().scaleEffect(0.4).padding(-7)
            }
        }
    }
    
    @State private var settingsWindow: WindowController<SettingsView>?
    
    var body: some View {
        TabView {
            Tab {
                StructureView(viewConfig: server.viewConfig, structure: structure, selectedEntryIds: $selectedEntryIds, lastSelectedEntry: lastSelectedEntry, searchField: AnyView(searchField))
            } label: {
                Text("Structure")
            }
            Tab {
                SequenceView(viewConfig: server.viewConfig, filteredEntries: $filteredEntries, selectedEntryIds: $selectedEntryIds, lastSelectedEntry: lastSelectedEntry, searchField: AnyView(searchField))
            } label: {
                Text("Sequence")
            }
        }
        .toolbar {
            ToolbarItemGroup {
                Button {
                    entries = []
                    filteredEntries = []
                    structure.hosts = []
                    selectedEntryIds = []
                } label: {
                    Image(systemName: "trash")
                }
                Button {
                    exporterPresented = true
                } label: {
                    Image(systemName: "arrow.down.to.line.compact")
                }
                Button {
                    if settingsWindow == nil {
                        settingsWindow = WindowController(title: "設定", rootView: SettingsView(server: server))
                    }
                    settingsWindow!.showWindow(nil)
                } label: {
                    Image(systemName: "gearshape")
                }
            }
        }
        .toolbar(removing: .sidebarToggle)
        .onChange(of: searchFilters) { _, newSearchFilters in
            isSearching = true
            Task {
                filteredEntries = newSearchFilters.isEmpty ? entries : entries.filter { entry in newSearchFilters.allSatisfy { f in f.match(entry: entry) } }
                structure = filteredEntries.reduce(into: Structure()) { structure, entry in structure.addEntry(entry) }
                isSearching = false
            }
        }
        .fileExporter(isPresented: $exporterPresented,
                      document: HARFile(entries: !selectedEntryIds.isEmpty ? filteredEntries.filter { selectedEntryIds.contains($0.id) } : filteredEntries),
                      contentType: UTType("public.har")!,
                      defaultFilename: "Untitled.har") { result in
            switch result {
            case .success(let url):
                print("\(url)に保存が完了しました！")
            case .failure(let error):
                print(error.localizedDescription)
            }
        }
        .onAppear {
            server.ipcServer.addEntryListener(id: id) { entry in
                Task {
                    entries.append(entry)
                    if searchFilters.isEmpty || searchFilters.allSatisfy({ $0.match(entry: entry) }) {
                        filteredEntries.append(entry)
                        structure.addEntry(entry)
                    }
                }
            }
        }
        .onDisappear {
            server.ipcServer.removeEntryListener(id: id)
            settingsWindow?.close()
        }
    }
}
