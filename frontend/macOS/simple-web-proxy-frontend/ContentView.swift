//
//  ContentView.swift
//  simple-web-proxy-frontend
//
//  Created by 富永康太 on 2026/07/19.
//

import SwiftUI

struct ContentView: View {
    @State var server: Server?
    @State var permissionRequestedAddress: String?
    
    var body: some View {
        Group {
            if let server = server {
                LogView(server: server)
                    .popover(isPresented: Binding(get: { permissionRequestedAddress != nil }, set: { _ in })) {
                        VStack {
                            Text("do_you_allow_\(permissionRequestedAddress!)_to_connect")
                            HStack {
                                Spacer()
                                Button("deny", role: .cancel) {
                                    permissionRequestedAddress = nil
                                }
                                Button("allow", role: .destructive) {
                                    do {
                                        let serverConfig = try ServerConfigObject.load(from: server.serverConfigJsonURL)
                                        serverConfig.acl.fields.append(.init(on: true, value: permissionRequestedAddress!))
                                        try serverConfig.save(to: server.serverConfigJsonURL)
                                    } catch let error {
                                        print(error)
                                    }
                                    permissionRequestedAddress = nil
                                }
                            }
                        }
                        .padding()
                    }
            } else {
                StartView { ipcPort, serverConfigJsonURL, requiresPermissionRequestOrNot in
                    server = Server(ipcPort: ipcPort, serverConfigJsonURL: serverConfigJsonURL, viewConfig: ViewConfigObject.new(), requiresPermissionRequestOrNot: requiresPermissionRequestOrNot)
                    server!.start()
                }
            }
        }
        .onDisappear {
            server?.terminate()
        }
    }
}

#Preview {
    ContentView()
}
