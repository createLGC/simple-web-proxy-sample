//
//  ProjectSettingsView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/11/28.
//

import SwiftUI

public struct ServerConfigView: View {
    @Bindable private var serverConfig: ServerConfigObject
    private let show_acl: Bool
    private let onChange: (() -> Void)?
    
    public init(serverConfig: ServerConfigObject, show_acl: Bool, onChange: (() -> Void)? = nil) {
        self.serverConfig = serverConfig
        self.show_acl = show_acl
        self.onChange = onChange
    }
    
    public var body: some View {
        HSplitView {
            VStack(spacing: 10) {
                if show_acl {
                    SwitchableFieldGroupTable(group: serverConfig.acl, label: "accessible_ip_addresses", valueHeader: "address", multiple: true, onChange: onChange)
                }
                SwitchableFieldGroupTable(group: serverConfig.sslProxying, label: "enable_ssl_proxying", valueHeader: "function", multiple: false, onChange: onChange)
                SwitchableFieldGroupTable(group: serverConfig.externalProxy, label: "enable_external_proxy", valueHeader: "function", multiple: false, onChange: onChange)
                SwitchableFieldGroupTable(group: serverConfig.modifyRequest, label: "enable_modify_request", valueHeader: "function", multiple: false, onChange: onChange)
                SwitchableFieldGroupTable(group: serverConfig.modifyResponse, label: "enable_modify_response", valueHeader: "function", multiple: false, onChange: onChange)
            }
            .frame(maxWidth: 240)
            ScriptEditView(scriptPath: $serverConfig.scriptPath)
                .onChange(of: serverConfig.scriptPath) {
                    if let onChange = onChange {
                        onChange()
                    }
                }
        }
        .padding()
    }
}
