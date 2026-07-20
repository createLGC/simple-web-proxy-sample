//
//  SwitchableFieldTable.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/11/28.
//

import SwiftUI

public struct SwitchableFieldGroupTable: View {
    struct ValueField: View {
        @Binding var text: String
        @State private var tmpText: String
        
        init(text: Binding<String>) {
            self._text = text
            self._tmpText = State(initialValue: text.wrappedValue)
        }
        
        var body: some View {
            TextField("", text: $tmpText)
                .onSubmit {
                    $text.wrappedValue = tmpText
                }
        }
    }
    
    private struct FieldKey: Equatable {
        let on: Bool
        let value: String
    }
    
    @Bindable var group: SwitchableFieldGroup
    
    let label: LocalizedStringKey
    let valueHeader: LocalizedStringKey
    let multiple: Bool
    
    var onChange: (() -> Void)? = nil
    
    @State var selectedFieldIds: Set<SwitchableField.ID> = []
    
    var header: some View {
        HStack {
            if group.on == nil {
                Text(label)
            } else {
                Toggle(isOn: Binding($group.on)!) {
                    Text(label)
                }
            }
            Spacer()
            Button(action: add) {
                Image(systemName: "plus").contentShape(Rectangle())
            }
            .buttonStyle(.plain)
            .disabled(group.on == false)
            Button(action: delete) {
                Image(systemName: "minus").contentShape(Rectangle())
            }
            .buttonStyle(.plain)
            .disabled(group.on == false)
        }
        .padding(.bottom, 1)
    }
    
    public var body: some View {
        VStack {
            header
            Table($group.fields, selection: $selectedFieldIds) {
                TableColumn(Text("valid")) { field in
                    Button {
                        if field.on.wrappedValue {
                            field.on.wrappedValue = false
                        } else {
                            if !multiple {
                                group.fields.forEach { $0.on = false }
                            }
                            field.on.wrappedValue = true
                        }
                    } label: {
                        Image(systemName: field.on.wrappedValue ? "checkmark.circle.fill" : "circle")
                            .foregroundColor(
                                group.on == false
                                    ? Color(nsColor: .tertiaryLabelColor)
                                    : $selectedFieldIds.wrappedValue.contains(field.id)
                                        ? .white
                                        : .accentColor
                            )
                    }
                    .buttonStyle(.plain)
                }
                .width(25)
                TableColumn(Text(valueHeader)) { field in
                    ValueField(text: field.value)
                }
            }
            .disabled(group.on == false)
        }
        .onChange(of: group.on) {
            reflectChange()
        }
        .onChange(of: group.fields.map { FieldKey(on: $0.on, value: $0.value) }) {
            reflectChange()
        }
    }
    
    func add() {
        group.fields.append(SwitchableField(on: false, value: ""))
    }
    
    func delete() {
        group.fields.removeAll(where: { selectedFieldIds.contains($0.id) })
    }
    
    func reflectChange() {
        if let onChange = onChange {
            onChange()
        }
    }
}
