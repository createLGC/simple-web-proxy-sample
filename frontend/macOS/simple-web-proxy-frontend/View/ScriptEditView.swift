//
//  ScriptEditView.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/08/07.
//

import SwiftUI
import UniformTypeIdentifiers

struct ScriptEditView: View {
    let watcher = FileWatcher()
    
    @Binding var scriptPath: String
    
    @State var initialScript = ""
    @State var script = ""
    @State var importerPresented = false
    @State var invalidScriptPath = false
    
    var body: some View {
        VStack {
            HStack {
                Text(scriptPath)
                    .textSelection(.enabled)
                    .padding(.leading, 8)
                Spacer()
                Button {
                    importerPresented = true
                } label: {
                    Image(systemName: "folder.fill")
                        .foregroundColor(.accentColor)
                }
                .buttonStyle(.plain)
                Button {
                    NSWorkspace.shared.open(URL(fileURLWithPath: scriptPath))
                } label: {
                    Image(systemName: "arrowshape.turn.up.forward.fill")
                        .foregroundColor(.accentColor)
                }
                .buttonStyle(.plain)
            }
            if invalidScriptPath {
                Text("failed_to_load_the_script_file")
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else {
                ZStack(alignment: .topTrailing) {
                    TextEditor(text: $script)
                        .font(.system(size: 16, design: .monospaced))
                    Button {
                        try! script.write(toFile: scriptPath, atomically: true, encoding: .utf8)
                        initialScript = script
                    } label: {
                        EmptyView()
                    }
                    .buttonStyle(.plain)
                    .keyboardShortcut("s", modifiers: .command)
                    if initialScript != script {
                        Circle()
                            .fill(Color.green)
                            .frame(width: 6, height: 6)
                            .padding(2)
                    }
                }
            }
        }
        .fileImporter(isPresented: $importerPresented, allowedContentTypes: [.pythonScript]) {
            switch($0) {
            case .success(let url):
                scriptPath = url.path(percentEncoded: false)
            case .failure:
                break
            }
        }
        .onAppear(perform: load)
        .onChange(of: scriptPath, load)
        .onDisappear(perform: watcher.end)
    }
    
    func reload() {
        do {
            initialScript = try String(contentsOfFile: scriptPath, encoding: .utf8)
            script = initialScript
            invalidScriptPath = false
        } catch {
            invalidScriptPath = true
        }
    }
    
    func load() {
        reload()
        watcher.start(filePath: scriptPath, reload)
    }
}
