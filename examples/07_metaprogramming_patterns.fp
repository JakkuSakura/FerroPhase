#!/usr/bin/env fp run
//! Metaprogramming patterns built on FerroPhase macros

fn main() {
    println!("=== Metaprogramming Patterns ===");

    //------------------------------------------------------------------
    // Pattern 1: Schema-driven ORM scaffolding
    //------------------------------------------------------------------
    struct TableField {
        name: &'static str,
        ty: &'static str,
        constraints: &'static str,
    }

    struct TableSchema {
        table: &'static str,
        fields: &'static [TableField],
    }

    const USER_SCHEMA: TableSchema = TableSchema {
        table: "users",
        fields: &[
            TableField { name: "id", ty: "u64", constraints: "PRIMARY KEY" },
            TableField { name: "name", ty: "String", constraints: "NOT NULL" },
            TableField { name: "email", ty: "String", constraints: "UNIQUE" },
            TableField { name: "created_at", ty: "u64", constraints: "DEFAULT NOW()" },
        ],
    };

    t! {
        struct User {
            id: u64,
            name: String,
            email: String,
            created_at: u64,

            fn insert(&self) {
                println!("INSERT INTO {} (name, email) VALUES ('{}', '{}')",
                    USER_SCHEMA.table, self.name, self.email);
            }

            fn find(id: u64) -> Option<User> {
                println!("SELECT * FROM {} WHERE id = {}", USER_SCHEMA.table, id);
                None
            }
        }
    };

    let alice = User {
        id: 1,
        name: "Alice".to_string(),
        email: "alice@example.com".to_string(),
        created_at: 0,
    };
    alice.insert();
    User::find(1);

    //------------------------------------------------------------------
    // Pattern 2: Domain-specific protocol messages
    //------------------------------------------------------------------
    struct MessageMetadata {
        code: u16,
        name: &'static str,
    }

    const MESSAGES: [MessageMetadata; 3] = [
        MessageMetadata { code: 0x01, name: "Ping" },
        MessageMetadata { code: 0x02, name: "Pong" },
        MessageMetadata { code: 0x10, name: "Data" },
    ];

    t! {
        enum ProtocolMessage {
            Ping,
            Pong,
            Data { payload_len: usize },

            fn opcode(&self) -> u16 {
                match self {
                    ProtocolMessage::Ping => 0x01,
                    ProtocolMessage::Pong => 0x02,
                    ProtocolMessage::Data { .. } => 0x10,
                }
            }
        }
    };

    const MESSAGE_VARIANTS: usize = MESSAGES.len();
    println!("Defined {} protocol metadata entries", MESSAGE_VARIANTS);

    for meta in &MESSAGES {
        println!("Message {:02X} => {}", meta.code, meta.name);
    }

    let payload = ProtocolMessage::Data { payload_len: 512 };
    println!("Payload opcode {:02X}", payload.opcode());

    println!("\n✓ Metaprogramming templates ready");
}
