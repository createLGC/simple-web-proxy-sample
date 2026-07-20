//
//  MultipartView.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/08/08.
//

import SwiftUI

struct MultipartItemView: View {
    let param: HAR_postData_param
    
    @State var isExpanded = true
    
    var body: some View {
        DisclosureGroup(isExpanded: $isExpanded) {
            VStack(alignment: .leading) {
                Grid {
                    GridRow {
                        Text(String("name")).font(.headline)
                        Text(param.name).fontDesign(.monospaced).textSelection(.enabled)
                    }
                    if let fileName = param.fileName {
                        GridRow {
                            Text(String("filename")).font(.headline)
                            Text(fileName).fontDesign(.monospaced).textSelection(.enabled)
                        }
                    }
                    if let contentType = param.contentType {
                        GridRow {
                            Text("Content-Type").font(.headline)
                            Text(contentType).fontDesign(.monospaced).textSelection(.enabled)
                        }
                    }
                }
                if let value = param.value {
                    if param._encoding != nil, let bytesData = Data(base64Encoded: value) {
                        BinaryView(data: bytesData)
                    } else {
                        Text(value).fontDesign(.monospaced).textSelection(.enabled)
                    }
                }
            }
        } label: {
            Text(param.name)
                .fontDesign(.monospaced)
                .contentShape(Rectangle())
                .frame(maxWidth: .infinity, alignment: .leading)
                .onTapGesture {
                    withAnimation {
                        isExpanded = !isExpanded
                    }
                }
        }
    }
}

struct MultipartView: View {
    let params: [HAR_postData_param]
    
    var body: some View {
        List(params) {
            MultipartItemView(param: $0)
        }
    }
}
