//
//  IpcServer.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/11/27.
//

import Foundation
import Network

class TCPServer {
    private let queue: DispatchQueue
    private let listener: NWListener
    
    var onTerminate: (() -> Void)?
    
    var isRunning: Bool {
        listener.state == .ready
    }
    
    init(port: NWEndpoint.Port.IntegerLiteralType) {
        queue = DispatchQueue.init(label: "com.kt.PyProxy.IPCServer")
        
        let params = NWParameters.tcp
        params.allowLocalEndpointReuse = true
        
        self.listener = try! NWListener(using: params, on: .init(integerLiteral: port))
        self.listener.stateUpdateHandler = { newState in
            switch newState {
            case .setup: print("listener setup")
            case .waiting(let error): print("listener waiting \(error)")
            case .ready: print("listener ready \(self.listener)")
            case .failed(let error):
                print("listener failed \(error)")
                self.listener.cancel()
            case .cancelled:
                print("listener cancelled")
                if let onTerminate = self.onTerminate {
                    onTerminate()
                }
            @unknown default:
                fatalError("unsupported listener state")
            }
        }
        
        self.listener.newConnectionHandler = { connection in
            connection.stateUpdateHandler = { newState in
                switch newState {
                case .ready:
                    print("connection ready")
                    connection.receiveMessage { data, context, flag, error in
                        if let data = data {
                            self.onReceiveMessage(data)
                        }
                    }
                case .failed(let error):
                    print("connection failed \(error)")
                    connection.cancel()
                case .cancelled:
                    print("connection cancelled")
                case .setup:
                    print("connection setup")
                case .waiting(let error):
                    print("connection waiting \(error)")
                case .preparing:
                    print("connection preparing")
                @unknown default:
                    fatalError("unsupported connection state")
                }
            }
            connection.start(queue: self.queue)
        }
    }
    
    func start() {
        self.listener.start(queue: self.queue)
    }
    
    func stop() {
        self.listener.cancel()
    }
    
    func onReceiveMessage(_ data: Data) {}
}

class IPCServer: TCPServer {
    private var entryListeners: [UUID:(HAR_entry) -> Void] = [:]
    
    func addEntryListener(id: UUID, listener: @escaping (HAR_entry) -> Void) {
        entryListeners[id] = listener
    }
    
    func removeEntryListener(id: UUID) {
        entryListeners.removeValue(forKey: id)
    }
    
    var onReceivePermissionRequest: ((String) -> Void)?
    
    init(port: NWEndpoint.Port.IntegerLiteralType, onReceivePermissionRequest: ((String) -> Void)? = nil) {
        self.onReceivePermissionRequest = onReceivePermissionRequest
        super.init(port: port)
    }
    
    override func onReceiveMessage(_ data: Data) {
        guard let message = try? JSONDecoder().decode(IPCMessage.self, from: data) else { return }
        switch(message.type) {
        case .entry:
            if let entryData = message.content.data(using: .utf8),
               let entry = try? JSONDecoder().decode(HAR_entry.self, from: entryData) {
                entryListeners.values.forEach { $0(entry) }
            }
        case .permissionRequest:
            if let onReceivePermissionRequest = onReceivePermissionRequest {
                onReceivePermissionRequest(message.content)
            }
        }
    }
}

struct IPCMessage: Codable {
    enum MessageType: String, Codable {
        case entry = "entry"
        case permissionRequest = "permissionRequest"
    }
    
    let type: MessageType
    let content: String
}
