import XCTest
import SwiftTreeSitter
import TreeSitterCrystal

final class TreeSitterCrystalTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_crystal())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Crystal grammar")
    }
}
