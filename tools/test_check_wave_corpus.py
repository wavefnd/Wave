#!/usr/bin/env python3

# This file is part of the Wave language project.
# Copyright (c) 2024–2026 Wave Foundation
# Copyright (c) 2024–2026 LunaStev and contributors
#
# This Source Code Form is subject to the terms of the
# Mozilla Public License, v. 2.0.
# If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.
#
# SPDX-License-Identifier: MPL-2.0
# AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

"""Unit tests for check_wave_corpus argument parsing."""

import argparse
import math
import sys
import unittest
from unittest.mock import patch

try:
    from tools.check_wave_corpus import parse_args
except ModuleNotFoundError:
    from check_wave_corpus import parse_args


class TestParseArgs(unittest.TestCase):
    """Test argument parsing with timeout validation."""

    def test_default_timeout(self):
        """Default timeout should be 15.0."""
        with patch.object(sys, "argv", ["check_wave_corpus.py"]):
            args = parse_args()
            self.assertEqual(args.timeout, 15.0)

    def test_valid_fractional_timeout(self):
        """Valid fractional timeout should be accepted."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=0.25"]):
            args = parse_args()
            self.assertEqual(args.timeout, 0.25)

    def test_valid_integer_timeout(self):
        """Valid integer timeout should be accepted."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=30"]):
            args = parse_args()
            self.assertEqual(args.timeout, 30.0)

    def test_zero_timeout_rejected(self):
        """Zero timeout should be rejected."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=0"]):
            with self.assertRaises(SystemExit) as context:
                parse_args()
            self.assertEqual(context.exception.code, 2)

    def test_negative_timeout_rejected(self):
        """Negative timeout should be rejected."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=-1"]):
            with self.assertRaises(SystemExit) as context:
                parse_args()
            self.assertEqual(context.exception.code, 2)

    def test_nan_timeout_rejected(self):
        """NaN timeout should be rejected."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=nan"]):
            with self.assertRaises(SystemExit) as context:
                parse_args()
            self.assertEqual(context.exception.code, 2)

    def test_inf_timeout_rejected(self):
        """Positive infinity timeout should be rejected."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=inf"]):
            with self.assertRaises(SystemExit) as context:
                parse_args()
            self.assertEqual(context.exception.code, 2)

    def test_negative_inf_timeout_rejected(self):
        """Negative infinity timeout should be rejected."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=-inf"]):
            with self.assertRaises(SystemExit) as context:
                parse_args()
            self.assertEqual(context.exception.code, 2)

    def test_non_numeric_timeout_rejected(self):
        """Non-numeric timeout should be rejected by argparse."""
        with patch.object(sys, "argv", ["check_wave_corpus.py", "--timeout=invalid"]):
            with self.assertRaises(SystemExit) as context:
                parse_args()
            self.assertEqual(context.exception.code, 2)


if __name__ == "__main__":
    unittest.main()
