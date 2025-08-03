use std::path::PathBuf;
use parser::parser::stdlib::StdlibManager;

/// Struct for managing Wave compiler configuration
#[derive(Debug)]
pub struct CompilerConfig {
    /// Whether Vex package manager integration is enabled
    pub vex_integration: bool,
    
    /// Standard library manager
    pub stdlib_manager: Option<StdlibManager>,
    
    /// Source file paths
    pub source_files: Vec<PathBuf>,
    
    /// Output file path
    pub output_path: Option<PathBuf>,
    
    /// Debug mode
    pub debug_mode: bool,
    
    /// Optimization level
    pub optimization_level: OptimizationLevel,
    
    /// Vex binary path (used in integration mode)
    pub vex_binary_path: Option<String>,
    
    /// Project root directory
    pub project_root: PathBuf,
}

#[derive(Debug, Clone, Copy)]
pub enum OptimizationLevel {
    None,
    O1,
    O2,
    O3,
}

impl CompilerConfig {
    /// Create new compiler configuration (default values)
    pub fn new() -> Self {
        Self {
            vex_integration: false,
            stdlib_manager: None,
            source_files: Vec::new(),
            output_path: None,
            debug_mode: false,
            optimization_level: OptimizationLevel::None,
            vex_binary_path: None,
            project_root: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
        }
    }

    /// Configure for Vex integration mode
    pub fn with_vex_integration(mut self, vex_binary_path: String) -> Self {
        self.vex_integration = true;
        self.vex_binary_path = Some(vex_binary_path);
        
        // TODO: Implement actual Vex binary path validation and communication setup
        // Initialize standard library manager
        let mut stdlib_manager = StdlibManager::new();
        stdlib_manager.enable_vex_integration(None);
        self.stdlib_manager = Some(stdlib_manager);
        
        self
    }

    /// Configure for standalone mode (no standard library)
    pub fn standalone_mode(mut self) -> Self {
        self.vex_integration = false;
        self.stdlib_manager = None;
        self.vex_binary_path = None;
        self
    }

    /// Add source file
    pub fn add_source_file(mut self, path: PathBuf) -> Self {
        self.source_files.push(path);
        self
    }

    /// Set output path
    pub fn with_output_path(mut self, path: PathBuf) -> Self {
        self.output_path = Some(path);
        self
    }

    /// Set debug mode
    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug_mode = debug;
        self
    }

    /// Set optimization level
    pub fn with_optimization(mut self, level: OptimizationLevel) -> Self {
        self.optimization_level = level;
        self
    }

    /// Return reference to standard library manager
    pub fn stdlib_manager(&self) -> Option<&StdlibManager> {
        self.stdlib_manager.as_ref()
    }

    /// Check compilation mode
    pub fn is_vex_mode(&self) -> bool {
        self.vex_integration
    }

    /// Check low-level mode (Wave standalone execution)
    pub fn is_low_level_mode(&self) -> bool {
        !self.vex_integration
    }

    /// Validate configuration settings
    pub fn validate(&self) -> Result<(), String> {
        if self.source_files.is_empty() {
            return Err("No source files specified".to_string());
        }

        for source_file in &self.source_files {
            if !source_file.exists() {
                return Err(format!("Source file does not exist: {}", source_file.display()));
            }
        }

        if self.vex_integration && self.vex_binary_path.is_none() {
            return Err("Vex integration enabled but no Vex binary path specified".to_string());
        }

        // TODO: Add actual Vex binary existence and version validation
        // TODO: Add project structure validation for Vex mode

        Ok(())
    }

    /// Print compilation information
    pub fn print_info(&self) {
        println!("Wave Compiler Configuration:");
        println!("  Mode: {}", if self.vex_integration { "Vex Integration" } else { "Standalone (Low-level)" });
        println!("  Source files: {}", self.source_files.len());
        for (i, file) in self.source_files.iter().enumerate() {
            println!("    {}: {}", i + 1, file.display());
        }
        
        if let Some(output) = &self.output_path {
            println!("  Output: {}", output.display());
        } else {
            println!("  Output: <default>");
        }
        
        println!("  Debug mode: {}", self.debug_mode);
        println!("  Optimization: {:?}", self.optimization_level);
        
        if self.vex_integration {
            println!("  Standard library: Available via Vex");
            if let Some(vex_path) = &self.vex_binary_path {
                println!("  Vex binary: {}", vex_path);
            }
        } else {
            println!("  Standard library: Not available (low-level mode)");
        }
    }
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self::new()
    }
}