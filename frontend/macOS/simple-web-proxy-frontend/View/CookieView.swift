//
//  CookieView.swift
//  Scriptable Proxy
//
//  Created by tester on 2023/12/01.
//

import SwiftUI

struct CookieView: View {
    let cookies: [HAR_cookie]
    
    @State var selectedCookieId: HAR_cookie.ID? = nil
    
    var body: some View {
        Table(cookies, selection: $selectedCookieId) {
            TableColumn("Key") {
                Text($0.name)
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
            TableColumn("Value") {
                Text($0.value)
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
            TableColumn("Path") {
                Text($0.path ?? String(localized: "no"))
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
            TableColumn("Domain") {
                Text($0.domain ?? String(localized: "no"))
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
            TableColumn("Expires") {
                Text($0.expires ?? String(localized: "no"))
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
            TableColumn("HttpOnly") {
                Text($0.httpOnly == true ? "yes" : "no")
            }
            TableColumn("Secure") {
                Text($0.secure == true ? "yes" : "no")
            }
            TableColumn("SameSite") {
                Text($0.sameSite ?? String(localized: "no"))
                    .lineLimit(nil)
                    .textSelection(.enabled)
            }
        }
    }
}
