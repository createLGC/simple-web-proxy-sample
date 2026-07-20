//
//  SettingsView.swift
//  simple-web-proxy-frontend
//
//  Created by 富永康太 on 2026/07/20.
//

import SwiftUI

struct SettingsView: View {
    @ObservedObject var server: Server
    
    var body: some View {
        TabView {
            Tab {
                VStack {
                    Toggle("requires_permission_request_or_not", isOn: $server.requiresPermissionRequestOrNot)
                    Spacer()
                }
                .padding()
            } label: {
                Text("general")
            }
            Tab {
                if let serverConfig = server.serverConfig {
                    ServerConfigView(serverConfig: serverConfig, show_acl: true) {
                        try? serverConfig.save(to: server.serverConfigJsonURL)
                    }
                } else {
                    Text("failed_to_load_server_config_json")
                }
            } label: {
                Text("server")
            }
            Tab {
                ViewConfigView(viewConfig: server.viewConfig)
            } label: {
                Text("view")
            }
        }
    }
}
