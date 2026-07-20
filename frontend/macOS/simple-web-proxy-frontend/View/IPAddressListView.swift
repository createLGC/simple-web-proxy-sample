//
//  IPAddressListView.swift
//  PyProxy
//
//  Created by tester on 2025/11/29.
//

import SwiftUI

public struct IPAddressListView: View {
    private class IPAddressObject: Identifiable {
        var id: String { name }
        let name: String
        let address: String
        
        init(name: String, address: String) {
            self.name = name
            self.address = address
        }
    }
    
    @State private var ipAddresses: [IPAddressObject] = []
    
    @State private var selectedAddressIds: Set<IPAddressObject.ID> = []
    
    private var copyStrings: [String] {
        ipAddresses.filter { selectedAddressIds.contains($0.id) }.map { $0.address }
    }
    
    public init() {}
    
    public var body: some View {
        VStack(spacing: 0) {
            Text("local_ip_address").padding(4)
            Table(ipAddresses, selection: $selectedAddressIds) {
                TableColumn(Text("name"), value: \.name).width(max: 40)
                TableColumn(Text("address"), value: \.address)
            }
            .copyable(copyStrings)
            .frame(minWidth: 200, minHeight: 200)
        }
        .onAppear {
            ipAddresses = enumerateIPAddresses()
        }
    }
    
    private func enumerateIPAddresses() -> [IPAddressObject] {
        var ifaListPtr: UnsafeMutablePointer<ifaddrs>? = nil
        guard getifaddrs(&ifaListPtr) == 0 else { return [] }
        defer {
            freeifaddrs(ifaListPtr)
        }
        
        var addresses: [IPAddressObject] = []
        
        var ifaPtr: UnsafeMutablePointer<ifaddrs>? = ifaListPtr
        while ifaPtr != nil {
            guard let ifa = ifaPtr?.pointee else { break }
            if ifa.ifa_addr.pointee.sa_family == Int32(AF_INET) {
                if let ifaName = String(validatingUTF8: ifa.ifa_name) {
                    let ifaAddress = ifa.ifa_addr.withMemoryRebound(to: sockaddr_in.self, capacity: 1) {
                        String(cString: inet_ntoa($0.pointee.sin_addr))
                    }
                    addresses.append(IPAddressObject(name: ifaName, address: ifaAddress))
                }
            }
            ifaPtr = ifa.ifa_next
        }
        
        return addresses
    }
}
