package tree_sitter_crystal_test

import (
	"testing"

	tree_sitter_crystal "github.com/keidax/tree-sitter-crystal/bindings/go"
	tree_sitter "github.com/tree-sitter/go-tree-sitter"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_crystal.Language())
	if language == nil {
		t.Errorf("Error loading Crystal grammar")
	}
}
