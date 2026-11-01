# SPDX-License-Identifier: MPL-2.0
"""Validated GitHub environment records shared by export and refresh."""

import os
import re
import uuid

NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\Z")


def name(value):
    if not isinstance(value, str) or not NAME.fullmatch(value):
        raise ValueError("invalid environment variable name")
    return value


def encode(key, value):
    name(key)
    value = str(value)
    if "\x00" in value:
        raise ValueError("environment values cannot contain NUL")
    if "\n" not in value and "\r" not in value:
        return f"{key}={value}\n".encode()
    delimiter = "wave_" + uuid.uuid4().hex
    while delimiter in value.splitlines():
        delimiter = "wave_" + uuid.uuid4().hex
    return f"{key}<<{delimiter}\n{value}\n{delimiter}\n".encode()


def decode(data):
    text = data.decode("utf-8-sig")
    result = {}
    lines = text.split("\n")
    index = 0
    while index < len(lines):
        line = lines[index].removesuffix("\r")
        index += 1
        if not line:
            continue
        if "=" in line and ("<<" not in line or line.index("=") < line.index("<<")):
            key, value = line.split("=", 1)
        elif "<<" in line:
            key, delimiter = line.split("<<", 1)
            if not delimiter or any(character in delimiter for character in "\r\n\x00"):
                raise ValueError("invalid environment delimiter")
            start = index
            while index < len(lines) and lines[index].removesuffix("\r") != delimiter:
                index += 1
            if index == len(lines):
                raise ValueError("unterminated environment record")
            # The last LF separates the value from its delimiter; every earlier
            # byte, including CRLF and trailing empty value lines, is payload.
            value = "\n".join(lines[start:index])
            index += 1
        else:
            raise ValueError("invalid environment record")
        name(key)
        if "\x00" in value:
            raise ValueError("environment values cannot contain NUL")
        result[key] = value
    return result


def append(path, record):
    # Validate before touching either the file or the caller's environment.
    decode(record)
    with open(path, "a+b", buffering=0) as stream:
        stream.seek(0, os.SEEK_END)
        original = stream.tell()
        if original:
            stream.seek(-1, os.SEEK_END)
            if stream.read(1) != b"\n":
                record = b"\n" + record
        try:
            offset = 0
            while offset < len(record):
                written = stream.write(record[offset:])
                if not written:
                    raise OSError("short environment record write")
                offset += written
            stream.flush()
        except BaseException:
            stream.truncate(original)
            raise
