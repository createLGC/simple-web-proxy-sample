import functools, gzip, zlib, brotli as br, zstd

def compress(data: bytes, encoding: str) -> bytes:
    if encoding == 'gzip':
        return gzip.compress(data)
    elif encoding == 'deflate':
        return zlib.compress(data)
    elif encoding == 'deflate-raw':
        compressor = zlib.compressobj(wbits=zlib.MAX_WBITS)
        compressed = compressor.compress(data)
        compressed += compressor.flush()
        return compressed
    elif encoding == 'br':
        return br.compress(data)
    elif encoding == 'zstd':
        return zstd.compress(data)
    else:
        return data

def decompress(data: bytes, encoding: str) -> tuple[bytes, str]:
    if encoding == 'gzip':
        return gzip.decompress(data), 'gzip'
    elif encoding == 'deflate':
        try:
            return zlib.decompress(data), 'deflate'
        except zlib.error:
            decompressor = zlib.decompressobj(wbits=-zlib.MAX_WBITS)
            decompressed = decompressor.decompress(data)
            decompressed += decompressor.flush()
            return decompressed, 'deflate-raw'
    elif encoding == 'br':
        return br.decompress(data), 'br'
    elif encoding == 'zstd':
        return zstd.decompress(data), 'zstd'
    else:
        return data

def decompress_with_codings(data: bytes, codings: list[str]) -> tuple[bytes, list[str]]:
    actual_codings = []
    for coding in reversed(codings):
        data, actual_coding = decompress(data, coding)
        actual_codings.append(actual_coding)
    return data, reversed(actual_codings)

def compress_with_codings(data: bytes, codings: list[str]) -> bytes:
    return functools.reduce(lambda data, encoding: compress(data, encoding), codings, data)