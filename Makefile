all:

run:
	cargo run compiler

test:
	cargo t  -- --nocapture