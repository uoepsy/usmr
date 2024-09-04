# Define variables
CONFIG_A = _quarto_GH.yml
CONFIG_B = _quarto_SUBS.yml
QUARTO_YML = _quarto.yml

# Pattern rule for rendering with CONFIG_A
%_A:
	@echo "Using $(CONFIG_A) to render $*.qmd..."
	@cp $(CONFIG_A) $(QUARTO_YML)
	@quarto render $*.qmd

# Pattern rule for rendering with CONFIG_B
%_B:
	@echo "Using $(CONFIG_B) to render $*.qmd..."
	@cp $(CONFIG_B) $(QUARTO_YML)
	@quarto render $*.qmd

# Combined target to render both versions
%: %_A %_B
	@echo "Restoring $(CONFIG_A) to $(QUARTO_YML)..."
	@cp $(CONFIG_A) $(QUARTO_YML)

# Clean up generated files
clean:
	@echo "Cleaning up generated files..."
	@rm -rf output_*_A output_*_B
	@rm -f $(QUARTO_YML)

.PHONY: clean
