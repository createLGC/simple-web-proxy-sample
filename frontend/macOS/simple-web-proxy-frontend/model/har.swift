//
//  har.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/11/30.
//

import Foundation
import AppKit
import SwiftUI
import UniformTypeIdentifiers

/// 仕様: https://w3c.github.io/web-performance/specs/HAR/Overview.html

struct HARFile: FileDocument {
    static var readableContentTypes: [UTType] { [UTType("public.har")!, .json] }
    static var writableContentTypes: [UTType] { [UTType("public.har")!, .json] }
    
    var har: HAR
    
    init(entries: [HAR_entry]) {
        har = HAR(log: HAR_log(version: "1.2", creator: HAR_creator(name: "Scriptable Proxy", version: "1.0", comment: nil), browser: nil, pages: nil, entries: entries, comment: nil))
    }
    
    init(configuration: ReadConfiguration) throws {
        if let data = configuration.file.regularFileContents {
            har = try JSONDecoder().decode(HAR.self, from: data)
        } else {
            har = HAR(log: HAR_log(version: "1.2", creator: HAR_creator(name: "Scriptable Proxy", version: "1.0", comment: nil), browser: nil, pages: nil, entries: [], comment: nil))
        }
    }
    
    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        return FileWrapper(regularFileWithContents: try JSONEncoder().encode(har))
    }
}

struct HAR: Codable {
    let log: HAR_log
}

struct HAR_log: Codable {
    let version: String
    let creator: HAR_creator
    let browser: HAR_browser?
    let pages: [HAR_page]?
    let entries: [HAR_entry]
    let comment: String?
}

struct HAR_creator: Codable {
    let name: String
    let version: String
    let comment: String?
}

struct HAR_browser: Codable {
    let name: String
    let version: String
    let comment: String?
}

struct HAR_page: Codable {
    let startedDateTime: String
    let id: String
    let title: String
    let pageTimings: HAR_pageTimings
    let comment: String?
}

struct HAR_pageTimings: Codable {
    let onContentLoad: String?
    let onLoad: String?
    let comment: String?
}

struct HAR_entry: Codable, Identifiable, Equatable {
    static func == (lhs: HAR_entry, rhs: HAR_entry) -> Bool {
        lhs.id == rhs.id
    }
    
    let id = UUID()
    
    let pageref: String?
    let startedDateTime: String
    let time: Double
    let request: HAR_request
    let response: HAR_response
    let cache: HAR_cache
    let timings: HAR_timings
    let _clientIPAddress: String?
    let serverIPAddress: String?
    let connection: String?
    let comment: String?
    
    enum CodingKeys: CodingKey {
        case pageref
        case startedDateTime
        case time
        case request
        case response
        case cache
        case timings
        case _clientIPAddress
        case serverIPAddress
        case connection
        case comment
    }
    
    var host: String? {
        request.headers.first { $0.name.lowercased() == "host" }?.value ?? request.urlComponents?.host
    }
}

struct HAR_request: Codable {
    let method: String
    let url: String
    let httpVersion: String
    let cookies: [HAR_cookie]
    let headers: [HAR_header]
    let queryString: [HAR_queryString_item]
    let postData: HAR_postData?
    let headersSize: Int
    let bodySize: Int
    let comment: String?
    
    var urlComponents: URLComponents? {
        URLComponents(string: url.starts(with: /https?\:\/\//) ? url : "https://\(url)")
    }
    
    var hostAndPort: String? {
        guard let urlComponents = urlComponents else { return nil }
        return "\(urlComponents.host ?? ""):\(urlComponents.port ?? 443)"
    }
    
    var pathAndQuery: String {
        guard let urlComponents = urlComponents else { return url }
        let query = urlComponents.query
        return urlComponents.path + (query == nil ? "" : "?\(query!)")
    }
    
    var requestLine: String {
        if method == "CONNECT" {
            if let hostAndPort = hostAndPort {
                return "\(method) \(hostAndPort) \(httpVersion)\r\n"
            } else {
                return "\(method) \(url) \(httpVersion)\r\n"
            }
        } else {
            return "\(method) \(pathAndQuery) \(httpVersion)\r\n"
        }
    }
    
    var headerString: String {
        requestLine + headers.map { $0.string }.joined(separator: "")
    }
    
    var string: String {
        let headerPart = headers.reduce("") { $0 + $1.string }
        let body = postData?.stringify(boundary: boundary) ?? ""
        return "\(requestLine)\(headerPart)\r\n\(body)"
    }
    
    var contentType: String? {
        headers.first(where: { $0.name.lowercased() == "content-type" })?.value
    }
    
    var boundary: String? {
        if let contentType = contentType, contentType.hasPrefix("multipart/form-data") {
            return HTTPValueWithOptions(string: contentType).options.get("boundary")
        } else {
            return nil
        }
    }
}

struct HAR_response: Codable {
    let status: Int
    let statusText: String
    let httpVersion: String
    let cookies: [HAR_cookie]
    let headers: [HAR_header]
    let content: HAR_content
    let redirectURL: String
    let headersSize: Int
    let bodySize: Int
    let comment: String?
    
    var statusLine: String {
        "\(httpVersion) \(status) \(statusText)\r\n"
    }
    
    var headerString: String {
        statusLine + headers.map { $0.string }.joined(separator: "")
    }
    
    var string: String {
        let headerPart = headers.reduce("") { $0 + $1.string }
        let body = content.text ?? ""
        return "\(statusLine)\(headerPart)\r\n\(body)"
    }
    
    var contentType: String? {
        headers.first(where: { $0.name.lowercased() == "content-type" })?.value
    }
}

struct HAR_cookie: Codable, Identifiable {
    let id = UUID()
    
    let name: String
    let value: String
    let path: String?
    let domain: String?
    let expires: String?
    let httpOnly: Bool?
    let secure: Bool?
    let sameSite: String?
    let comment: String?
    
    enum CodingKeys: CodingKey {
        case name
        case value
        case path
        case domain
        case expires
        case httpOnly
        case secure
        case sameSite
        case comment
    }
}

struct HAR_header: Codable, Identifiable {
    let id = UUID()
    
    let name: String
    let value: String
    let comment: String?
    
    enum CodingKeys: CodingKey {
        case name
        case value
        case comment
    }
    
    var string: String { "\(name): \(value)\r\n" }
}

struct HAR_queryString_item: Codable, Identifiable {
    let id = UUID()
    
    let name: String
    let value: String
    let comment: String?
    
    enum CodingKeys: CodingKey {
        case name
        case value
        case comment
    }
}

struct HAR_postData: Codable {
    let mimeType: String
    let params: [HAR_postData_param]
    let text: String
    let _encoding: String?
    let comment: String?
    
    func stringify(boundary: String? = nil) -> String {
        if mimeType.hasPrefix("application/x-www-form-urlencoded") {
            return params.map { $0.stringify() }.joined(separator: "&")
        } else if mimeType.hasPrefix("multipart/form-data"), let boundary = boundary {
            return params.reduce("") { $0 + "\(boundary)\r\n\($1.stringify(multipart: true))" } + "--\(boundary)--\r\n"
        } else if mimeType.lowercased().hasPrefix("application/json"),
                  let data = text.data(using: .utf8),
                  let json = try? JSONSerialization.jsonObject(with: data),
                  let formattedData = try? JSONSerialization.data(withJSONObject: json, options: [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]) {
            return String(data: formattedData, encoding: .utf8) ?? text
        } else {
            return text
        }
    }
    
    var filenameExtension: String {
        let _extension = UTType(mimeType: mimeType)?.preferredFilenameExtension
        return _extension != nil ? "." + _extension! : ""
    }
}

struct HAR_postData_param: Codable, Identifiable {
    let id = UUID()
    
    let name: String
    let value: String?
    let fileName: String?
    let contentType: String?
    let _encoding: String?
    let comment: String?
    
    enum CodingKeys: CodingKey {
        case name
        case value
        case fileName
        case contentType
        case _encoding
        case comment
    }
    
    func stringify(multipart: Bool = false) -> String {
        if multipart {
            var headerString = "Content-Disposition: form-data; name=\(name)"
            if let fileName = fileName {
                headerString += "; filename=\(fileName)"
            }
            headerString += "\r\n"
            if let contentType = contentType {
                headerString += "Content-Type: \(contentType)\r\n"
            }
            headerString += "\r\n"
            if let value = value {
                return "\(headerString)\(value)\r\n"
            } else {
                return headerString
            }
        } else {
            return "\(name)=\(value ?? "")"
        }
    }
}

struct HAR_content: Codable {
    let size: Int
    let compression: Int?
    let mimeType: String
    let text: String?
    let encoding: String?
    let comment: String?
    
    var string: String? {
        if mimeType.hasPrefix("application/json"),
           let data = text?.data(using: .utf8),
           let json = try? JSONSerialization.jsonObject(with: data),
           let formattedData = try? JSONSerialization.data(withJSONObject: json, options: [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]) {
            return String(data: formattedData, encoding: .utf8)
        }
        return text
    }
    
    var image: NSImage? {
        if mimeType.hasPrefix("image"),
           encoding == "base64",
           let text = text,
           let data = Data(base64Encoded: text, options: Data.Base64DecodingOptions(rawValue: 0)) {
            return NSImage(data: data)
        }
        return nil
    }
    
    var html: String? {
        mimeType.hasPrefix("text/html") ? text : nil
    }
    
    var filenameExtension: String {
        let _extension = UTType(mimeType: mimeType)?.preferredFilenameExtension
        return _extension != nil ? "." + _extension! : ""
    }
}

struct HAR_cache: Codable {
    let beforeRequest: HAR_cache_item?
    let afterRequest: HAR_cache_item?
    let comment: String?
}

struct HAR_cache_item: Codable {
    let expires: String?
    let lastAccess: String
    let eTag: String
    let hitCount: Int
    let comment: String?
}

struct HAR_timings: Codable {
    let blocked: Double?
    let dns: Double?
    let connect: Double?
    let send: Double
    let wait: Double
    let receive: Double
    let ssl: Double?
    let comment: String?
}
