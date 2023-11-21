all:
	cargo build
run:
	cargo run compiler

test:
	cargo t  -- --nocapture