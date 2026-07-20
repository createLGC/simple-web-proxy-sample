//
//  Server.swift
//  Scriptable Proxy
//
//  Created by tester on 2025/11/27.
//

import Foundation
import SwiftUI
import Combine
import Network

public final class Server: ObservableObject {
    let ipcServer: IPCServer
    let watcher: FileWatcher
    
    let serverConfigJsonURL: URL
    @Published var serverConfig: ServerConfigObject?
    let viewConfig: ViewConfigObject
    @Published var requiresPermissionRequestOrNot: Bool {
        willSet {
            if newValue {
                ipcServer.onReceivePermissionRequest = {
                    self.permissionRequestedAddress = $0
                }
            }
        }
    }
    @Published var permissionRequestedAddress: String? = nil
    
    public init(ipcPort: NWEndpoint.Port.IntegerLiteralType, serverConfigJsonURL: URL, viewConfig: ViewConfigObject, requiresPermissionRequestOrNot: Bool) {
        ipcServer = IPCServer(port: ipcPort)
        watcher = FileWatcher()
        
        self.serverConfigJsonURL = serverConfigJsonURL
        self.viewConfig = viewConfig
        self.requiresPermissionRequestOrNot = requiresPermissionRequestOrNot
    }
    
    public func start() {
        serverConfig = try? ServerConfigObject.load(from: serverConfigJsonURL)
        
        watcher.start(filePath: serverConfigJsonURL.path(percentEncoded: false)) {
            let serverConfig = try? ServerConfigObject.load(from: self.serverConfigJsonURL)
            DispatchQueue.main.async {
                self.serverConfig = serverConfig
            }
        }
        
        ipcServer.start()
    }
    
    public func terminate() {
        ipcServer.stop()
        watcher.end()
    }
}
