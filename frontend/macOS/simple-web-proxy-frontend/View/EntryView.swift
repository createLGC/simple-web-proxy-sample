//
//  RequestView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/11/30.
//

import SwiftUI

struct EntryView: View {
    let entry: HAR_entry
    let viewConfig: ViewConfigObject
    
    var requestView: some View {
        VStack(alignment: .leading, spacing: 0) {
            Text("Request").padding(8)
            TabView {
                Tab {
                    RawView(value: entry.request.string)
                } label: {
                    Text("Raw")
                }
                Tab {
                    HeaderView(headers: entry.request.headers)
                } label: {
                    Text("Header")
                }
                if !entry.request.queryString.isEmpty {
                    Tab {
                        QueryStringView(queryString: entry.request.queryString)
                    } label: {
                        Text("Query String")
                    }
                }
                if !entry.request.cookies.isEmpty {
                    Tab {
                        CookieView(cookies: entry.request.cookies)
                    } label: {
                        Text("Cookie")
                    }
                }
                if let postData = entry.request.postData {
                    let contentType = entry.request.contentType
                    if contentType?.hasPrefix("application/x-www-form-urlencoded") == true {
                        Tab {
                            FormView(params: postData.params)
                        } label: {
                            Text("Form")
                        }
                    } else if contentType?.hasPrefix("multipart/form-data") == true {
                        Tab {
                            MultipartView(params: postData.params)
                        } label: {
                            Text("Multipart")
                        }
                    } else {
                        let _filename = entry.request.urlComponents?.url?.lastPathComponent ?? "request_body"
                        let filenameExtension = postData.filenameExtension
                        let filename = _filename.hasSuffix(filenameExtension) ? _filename : _filename + filenameExtension
                        Tab {
                            TextView(text: postData.stringify(), base64Encoded: postData._encoding != nil, filename: filename)
                        } label: {
                            Text("Body")
                        }
                        if postData._encoding != nil, let bytesData = Data(base64Encoded: postData.text) {
                            Tab {
                                BinaryView(data: bytesData)
                            } label: {
                                Text("Binary")
                            }
                        }
                    }
                }
                if let validScript = viewConfig.customRequestView.validFieldValue {
                    Tab {
                        CustomView(object: entry.request, script: viewConfig.script, function: validScript)
                    } label: {
                        Text("Custom")
                    }
                }
            }
            .tabViewStyle(.sidebarAdaptable)
            .toolbar(removing: .sidebarToggle)
        }
    }
    
    var responseView: some View {
        VStack(alignment: .leading, spacing: 0) {
            Text("Response").padding(8)
            TabView {
                Tab {
                    RawView(value: entry.response.string)
                } label: {
                    Text("Raw")
                }
                Tab {
                    HeaderView(headers: entry.response.headers)
                } label: {
                    Text("Header")
                }
                if !entry.response.cookies.isEmpty {
                    Tab {
                        CookieView(cookies: entry.response.cookies)
                    } label: {
                        Text("Set-Cookie")
                    }
                }
                if let image = entry.response.content.image {
                    Tab {
                        Image(nsImage: image)
                            .resizable()
                            .scaledToFit()
                            .frame(maxWidth: 300, maxHeight: 300)
                    } label: {
                        Text("Body")
                    }
                } else if let html = entry.response.content.html,
                          let urlComponents = URLComponents(string: entry.request.url),
                          let scheme = urlComponents.scheme,
                          let host = urlComponents.host,
                          let baseURL = URL(string: "\(scheme)://\(host)/") {
                    Tab {
                        HTMLView(html: html, baseURL: baseURL)
                    } label: {
                        Text("Body")
                    }
                } else if let body = entry.response.content.string, !body.isEmpty {
                    let _filename = entry.request.urlComponents?.url?.lastPathComponent ?? "response_body"
                    let filenameExtension = entry.response.content.filenameExtension
                    let filename = _filename.hasSuffix(filenameExtension) ? _filename : _filename + filenameExtension
                    Tab {
                        TextView(text: body, base64Encoded: entry.response.content.encoding != nil, filename: filename)
                    } label: {
                        Text("Body")
                    }
                }
                if entry.response.content.encoding != nil,
                   let text = entry.response.content.text,
                   let bytesData = Data(base64Encoded: text) {
                    Tab {
                        BinaryView(data: bytesData)
                    } label: {
                        Text("Binary")
                    }
                }
                if let validScript = viewConfig.customResponseView.validFieldValue {
                    Tab {
                        CustomView(object: entry.response, script: viewConfig.script, function: validScript)
                    } label: {
                        Text("Custom")
                    }
                }
            }
            .tabViewStyle(.sidebarAdaptable)
            .toolbar(removing: .sidebarToggle)
        }
    }
    
    var body: some View {
        requestView
        responseView
    }
}
