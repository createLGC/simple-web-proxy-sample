//
//  FileWatcher.swift
//  simple-web-proxy-frontend
//
//  Created by 富永康太 on 2026/07/20.
//

import Foundation

class FileWatcher {
    private var source: DispatchSourceFileSystemObject?
    
    func start(filePath: String, _ eventHandler: @escaping () -> Void) {
        let fd = open(filePath, O_EVTONLY)
        guard fd > -1 else { return }
        source = DispatchSource.makeFileSystemObjectSource(fileDescriptor: fd, eventMask: .write, queue: DispatchQueue.global())
        source!.setEventHandler {
            eventHandler()
        }
        source!.setCancelHandler {
            close(fd)
        }
        source!.activate()
    }
    
    func end() {
        guard source != nil else { return }
        source!.cancel()
        source = nil
    }
}
