//
//  ProjectViewSettingsView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/25.
//

import SwiftUI

public struct ViewConfigView: View {
    @Bindable private var viewConfig: ViewConfigObject
    
    public init(viewConfig: ViewConfigObject) {
        self.viewConfig = viewConfig
    }
    
    public var body: some View {
        HSplitView {
            VStack(spacing: 10) {
                ViewColumnsTable(viewColumns: viewConfig.columns)
                SwitchableFieldGroupTable(group: viewConfig.customRequestView, label: "requestView", valueHeader: "function", multiple: false)
                SwitchableFieldGroupTable(group: viewConfig.customResponseView, label: "responseView", valueHeader: "function", multiple: false)
            }
            .frame(maxWidth: 240)
            TextEditor(text: $viewConfig.script)
                .font(.system(size: 18, design: .monospaced))
        }
        .padding()
    }
}
