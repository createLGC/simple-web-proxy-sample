//
//  BinaryView.swift
//  Scriptable Proxy
//
//  Created by tester on 2024/07/30.
//

import SwiftUI

extension UInt8 {
    
    var hex: String {
        String(format: "%02X", self)
    }
    
    var ascii: String {
        switch(self) {
        case 0:
            return "\\0"
        case 7:
            return "\\a"
        case 8:
            return "\\b"
        case 9:
            return "\\t"
        case 10:
            return "\\n"
        case 11:
            return "\\v"
        case 12:
            return "\\f"
        case 13:
            return "\\r"
        default:
            return String(cString: [self, 0])
        }
    }
    
}

struct BinaryView: View {
    let data: Data
    
    let numberOfCols = 16
    
    var numberOfRows: Int {
        Int(ceil(Double(data.count) / Double(numberOfCols)))
    }
    
    var body: some View {
        ScrollView([.horizontal, .vertical]) {
            LazyVStack {
                ForEach(stride(from: 0, to: data.count, by: 16).map { $0 }, id: \.self) { start in
                    var row = String(format: "%08X", start) + "  "
                    for i in start..<start + 16 {
                        row += (i < data.count ? String(format: "%02X", data[i]) : "  ") + " "
                    }
                    row += " "
                    for i in start..<start + 16 {
                        row += i < data.count ? (32 < data[i] && data[i] < 127 ? String(cString: [data[i], 0]) : " ") : " "
                    }
                    return Text(row).font(.system(size: 14, design: .monospaced))
                }
            }
        }
        .background(Color(nsColor: .textBackgroundColor))
    }
}
