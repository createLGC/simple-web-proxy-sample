//
//  StartView.swift
//  simple-web-proxy-frontend
//
//  Created by 富永康太 on 2026/07/20.
//

import SwiftUI
import UniformTypeIdentifiers
import Network

struct StartView: View {
    @State var ipcPort = ""
    @State var serverConfigJsonURL: URL?
    @State var requiresPermissionRequestOrNot = true
    
    @State private var isImportingJson = false
    
    let onSubmit: (_ ipcPort: NWEndpoint.Port.IntegerLiteralType, _ serverConfigJsonURL: URL, _ requiresPermissionRequestOrNot: Bool) -> Void
    
    var body: some View {
        VStack(spacing: 20) {
            Grid(alignment: .leading, horizontalSpacing: 20, verticalSpacing: 20) {
                GridRow {
                    Text("ipc_port")
                    TextField("", text: $ipcPort)
                }
                GridRow {
                    Text("server_config_json_url")
                    HStack {
                        if let url = serverConfigJsonURL {
                            Text(url.path(percentEncoded: false))
                        } else {
                            Text("not_set")
                        }
                        Spacer()
                        Button {
                            isImportingJson = true
                        } label: {
                            Image(systemName: "folder.fill")
                                .foregroundStyle(Color.accentColor)
                        }
                        .buttonStyle(.plain)
                    }
                }
            }
            Toggle("requires_permission_request_or_not", isOn: $requiresPermissionRequestOrNot)
            if let ipcPort = NWEndpoint.Port.IntegerLiteralType(ipcPort), let serverConfigJsonURL = serverConfigJsonURL {
                Button("run") {
                    self.onSubmit(ipcPort, serverConfigJsonURL, requiresPermissionRequestOrNot)
                }
            }
        }
        .frame(maxWidth: 800)
        .fileImporter(isPresented: $isImportingJson, allowedContentTypes: [.json]) {
            switch $0 {
            case .success(let url):
                serverConfigJsonURL = url
            case .failure(let error):
                break
            }
        }
    }
}
