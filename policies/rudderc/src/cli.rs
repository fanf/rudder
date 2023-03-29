// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: 2022 Normation SAS

use std::path::PathBuf;

use clap::{Parser, Subcommand};

use crate::doc::Format;

/// Compile Rudder policies
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct MainArgs {
    /// Verbose
    #[arg(short, long, action = clap::ArgAction::Count)]
    pub verbose: u8,

    /// Quiet
    #[arg(short, long)]
    pub quiet: bool,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Create the technique structure in the current directory
    Init,

    /// Check the technique for errors
    Check {
        /// Load a library from the given path
        #[arg(short, long, action = clap::ArgAction::Append)]
        library: Vec<PathBuf>,
    },

    /// Build the technique
    Build {
        /// Load a library from the given path
        #[arg(short, long, action = clap::ArgAction::Append)]
        library: Vec<PathBuf>,

        /// Output directory
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Build the methods documentation
    Lib {
        /// Load a library from the given path
        #[arg(short, long, action = clap::ArgAction::Append)]
        library: Vec<PathBuf>,

        /// Output directory
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Output format
        #[arg(short, long)]
        #[arg(value_enum)]
        #[arg(default_value_t = Format::Html)]
        format: Format,

        /// Open in browser
        #[arg(long)]
        open: bool,

        /// Output in stdout
        #[arg(long)]
        stdout: bool,
    },
}
