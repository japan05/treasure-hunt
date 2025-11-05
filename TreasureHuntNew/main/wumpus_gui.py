import os
import sys
import time
import threading
import random
from dataclasses import dataclass
from typing import List, Tuple, Optional, Dict

try:
    import tkinter as tk
    from tkinter import ttk, filedialog, messagebox
except Exception:
    print("Tkinter is required to run this GUI.")
    sys.exit(1)

# Optional: PIL for better image handling
PIL_AVAILABLE = False
PIL_ERROR = None
try:
    from PIL import Image, ImageTk
    PIL_AVAILABLE = True
    print("✓ PIL/Pillow is available for image loading")
except ImportError as e:
    PIL_ERROR = str(e)
    print("⚠ PIL/Pillow not found - only GIF/PGM images will work")
    print(f"  Error: {e}")
    print("  Install with: py -m pip install pillow")
except Exception as e:
    PIL_ERROR = str(e)
    print(f"⚠ Error importing PIL: {e}")

# --- Game model --------------------------------------------------------------

STAT_PLAYER = "PLAYER"
STAT_START = "START"
STAT_WUMPUS = "WUMPUS"
STAT_HOLE = "HOLE"
STAT_WIND = "WIND"
STAT_STENCH = "STENCH"
STAT_GOLD = "GOLD"

MOVE_UP = "UP"
MOVE_DOWN = "DOWN"
MOVE_LEFT = "LEFT"
MOVE_RIGHT = "RIGHT"

@dataclass
class Cell:
    contents: List[str]

    def has(self, status: str) -> bool:
        return status in self.contents

class GameState:
    def __init__(self, width: int = 7, height: int = 7):
        self.width = width
        self.height = height
        self.grid: List[List[Cell]] = [[Cell([]) for _ in range(width)] for _ in range(height)]
        self.player_pos: Tuple[int, int] = (0, 0)
        self.has_gold: bool = False
        self.has_escaped: bool = False
        self.alive: bool = True
        self._place_start()

    def reset(self, width: Optional[int] = None, height: Optional[int] = None):
        if width is not None:
            self.width = width
        if height is not None:
            self.height = height
        self.grid = [[Cell([]) for _ in range(self.width)] for _ in range(self.height)]
        self.has_gold = False
        self.has_escaped = False
        self.alive = True
        self.player_pos = (0, 0)
        self._place_start()

    def _place_start(self):
        x, y = 0, 0
        self.grid[y][x].contents = [STAT_START, STAT_PLAYER]

    def setup_default_world(self):
        """Set up default world with fixed positions for testing"""
        # Clear all existing contents except start
        for y in range(self.height):
            for x in range(self.width):
                self.grid[y][x].contents = []
        
        # Place start and player at (0,0)
        self._place_start()
        self.player_pos = (0, 0)
        self.has_gold = False
        self.has_escaped = False
        self.alive = True
        
        # Default positions (you can modify these)
        wumpus_pos = (3, 4)    # Wumpus at (3,4)
        gold_pos = (6, 2)      # Gold at (6,2) 
        pit_positions = [(2, 3), (4, 1), (1, 5)]  # Multiple pits
        
        # Place Wumpus
        if self._is_valid_position(wumpus_pos[0], wumpus_pos[1]):
            self.grid[wumpus_pos[1]][wumpus_pos[0]].contents.append(STAT_WUMPUS)
            self._add_surrounding(wumpus_pos[1], wumpus_pos[0], STAT_STENCH)
        
        # Place Gold
        if self._is_valid_position(gold_pos[0], gold_pos[1]):
            self.grid[gold_pos[1]][gold_pos[0]].contents.append(STAT_GOLD)
        
        # Place Pits
        for pit_x, pit_y in pit_positions:
            if self._is_valid_position(pit_x, pit_y) and (pit_x, pit_y) != (0, 0):
                self.grid[pit_y][pit_x].contents.append(STAT_HOLE)
                self._add_surrounding(pit_y, pit_x, STAT_WIND)
    
    def _is_valid_position(self, x: int, y: int) -> bool:
        """Check if position is within grid bounds"""
        return 0 <= x < self.width and 0 <= y < self.height

    def randomize(self, wumpi: int = 1, holes: int = 1):
        # Clear all but keep start position
        for y in range(self.height):
            for x in range(self.width):
                self.grid[y][x].contents = []
        self._place_start()
        self.player_pos = (0, 0)
        self.has_gold = False
        self.has_escaped = False
        self.alive = True

        # Place one GOLD somewhere not at (0,0)
        gx, gy = self._rand_free_cell(exclude={(0, 0)})
        self.grid[gy][gx].contents.append(STAT_GOLD)

        # Place Wumpus and Holes
        excluded = {(0, 0), (gx, gy)}
        for _ in range(max(0, wumpi)):
            x, y = self._rand_free_cell(exclude=excluded)
            self.grid[y][x].contents.extend([STAT_WUMPUS])
            excluded.add((x, y))
            self._add_surrounding(y, x, STAT_STENCH)
        for _ in range(max(0, holes)):
            x, y = self._rand_free_cell(exclude=excluded)
            self.grid[y][x].contents.extend([STAT_HOLE])
            excluded.add((x, y))
            self._add_surrounding(y, x, STAT_WIND)

        # Put player back
        sx, sy = 0, 0
        if STAT_PLAYER not in self.grid[sy][sx].contents:
            self.grid[sy][sx].contents.extend([STAT_PLAYER, STAT_START])

    def _rand_free_cell(self, exclude: set) -> Tuple[int, int]:
        while True:
            x = random.randint(0, self.width - 1)
            y = random.randint(0, self.height - 1)
            if (x, y) in exclude:
                continue
            if not self.grid[y][x].contents:
                return x, y

    def _add_surrounding(self, y: int, x: int, status: str):
        for dy, dx in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            ny, nx = y + dy, x + dx
            if 0 <= ny < self.height and 0 <= nx < self.width:
                if status not in self.grid[ny][nx].contents:
                    self.grid[ny][nx].contents.append(status)

    def move_player(self, move: str):
        if not self.alive or self.has_escaped:
            return
        x, y = self.player_pos
        nx, ny = x, y
        if move == MOVE_UP:
            ny = max(0, y - 1)
        elif move == MOVE_DOWN:
            ny = min(self.height - 1, y + 1)
        elif move == MOVE_LEFT:
            nx = max(0, x - 1)
        elif move == MOVE_RIGHT:
            nx = min(self.width - 1, x + 1)
        if (nx, ny) != (x, y):
            # update grid flags
            if STAT_PLAYER in self.grid[y][x].contents:
                self.grid[y][x].contents.remove(STAT_PLAYER)
            self.player_pos = (nx, ny)
            if STAT_PLAYER not in self.grid[ny][nx].contents:
                self.grid[ny][nx].contents.append(STAT_PLAYER)
            # hazards
            if STAT_WUMPUS in self.grid[ny][nx].contents or STAT_HOLE in self.grid[ny][nx].contents:
                self.alive = False

    def pickup(self):
        x, y = self.player_pos
        if STAT_GOLD in self.grid[y][x].contents:
            self.grid[y][x].contents.remove(STAT_GOLD)
            self.has_gold = True

    def climb(self):
        if self.player_pos == (0, 0) and self.has_gold:
            self.has_escaped = True

    def get_self_status(self) -> List[str]:
        x, y = self.player_pos
        return self.grid[y][x].contents.copy()

# --- Bot communicator via text file (same protocol) -------------------------

# Import Prolog bridge
try:
    from prolog_bridge import PrologCommunicator
    PROLOG_AVAILABLE = True
except ImportError:
    PROLOG_AVAILABLE = False
    print("⚠ Prolog bridge not available - AI features disabled")

# --- GUI --------------------------------------------------------------------

# Modern light blue game theme
COLORS = {
    'bg_light': '#E0F2F7',           # Main light blue background
    'bg_panel': '#F0F8FA',           # Lighter panel background
    'bg_button': '#4A90E2',          # Modern blue button
    'bg_button_hover': '#5BA0F2',    # Button hover
    'bg_button_active': '#3A80D2',   # Button active
    'accent': '#FF6B6B',             # Accent color (coral red)
    'accent_gold': '#FFD93D',        # Gold color
    'text_dark': '#2C3E50',          # Dark text
    'text_light': '#FFFFFF',         # Light text for buttons
    'text_muted': '#7F8C8D',         # Muted text
    'grid_bg': '#FFFFFF',            # Grid background (white)
    'cell_bg': '#F8F9FA',            # Cell background
    'cell_border': '#DEE2E6',        # Cell border
    'success': '#51CF66',             # Success green
    'danger': '#FF6B6B',              # Danger red/coral
    'warning': '#FFD93D',             # Warning yellow
    'info': '#4A90E2',                # Info blue
    'border': '#BDC3C7',              # Borders
    'shadow': '#95A5A6',              # Shadows
}

# Modern font configuration
MODERN_FONTS = {
    'title': ("Segoe UI", 24, "bold"),
    'heading': ("Segoe UI", 18, "bold"),
    'subheading': ("Segoe UI", 14, "bold"),
    'body': ("Segoe UI", 11),
    'small': ("Segoe UI", 9),
    'button': ("Segoe UI", 11, "bold"),
}

# Fallback fonts if Segoe UI not available
FONT_FALLBACKS = ["Segoe UI", "Helvetica Neue", "SF Pro Display", "San Francisco", "Arial"]

class WumpusGUI(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("🏴‍☠️ Treasure Hunt - Wumpus World")
        self.geometry("1200x800")
        self.resizable(True, True)
        self.configure(bg=COLORS['bg_light'])

        self.gs = GameState(7, 7)
        self.gs.setup_default_world()  # Set up default world instead of empty
        self.buttons: List[List[tk.Button]] = []

        # Initialize instance variables BEFORE building controls
        self.prolog_communicator: Optional[PrologCommunicator] = None
        self.running = False
        self.period_ms = 1000
        self.ai_active = False
        self.player_direction = "DOWN"  # Default player facing direction
        self.game_over_popup_shown = False  # Track if game over popup has been shown

        # Configure ttk style - use 'clam' theme for better macOS compatibility
        self.style = ttk.Style()
        # Try to use 'clam' theme, fallback to default if not available
        try:
            self.style.theme_use('clam')
        except:
            # If 'clam' not available, try other themes
            try:
                self.style.theme_use('alt')
            except:
                pass  # Use system default
        self._configure_styles()

        # Main container
        self.main_container = tk.Frame(self, bg=COLORS['bg_light'])
        self.main_container.pack(fill=tk.BOTH, expand=True, padx=15, pady=15)

        # Top section: Title and status
        self._build_header()
        
        # Middle section: Game board and sidebar
        self.middle_container = tk.Frame(self.main_container, bg=COLORS['bg_light'])
        self.middle_container.pack(fill=tk.BOTH, expand=True, pady=(10, 0))
        
        # Left side: Game board with modern card design
        board_shadow, board_content, _ = self._create_modern_card(
            self.middle_container, radius=15, shadow_offset=4, bg=COLORS['grid_bg'])
        board_shadow.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=(0, 10))
        
        self.board_frame = board_content
        
        # Board title with modern styling
        title_container = tk.Frame(board_content, bg=COLORS['grid_bg'])
        title_container.pack(fill=tk.X, pady=(15, 10))
        
        board_title = tk.Label(title_container, text="Game Board", 
                              font=MODERN_FONTS['heading'],
                              bg=COLORS['grid_bg'], fg=COLORS['text_dark'])
        board_title.pack(side=tk.LEFT, padx=20)
        
        # Container for the grid with white background
        self.container = tk.Frame(board_content, bg=COLORS['grid_bg'],
                                 relief=tk.FLAT, bd=0)
        self.container.pack(expand=True, fill=tk.BOTH, padx=20, pady=(0, 20))
        
        # Center frame for the grid - use pack for stable positioning
        self.center = tk.Frame(self.container, bg=COLORS['grid_bg'])
        self.center.pack(expand=True)
        
        # Right side: Status panel
        self._build_status_panel()
        
        # Bottom section: Controls
        self._build_controls()

        # Initialize image loader
        self._init_images()

        self._build_grid()
        self._color_grid()
        
        # Add keyboard bindings for manual control
        self.bind("<Key>", self.on_key_press)
        self.focus_set()
        
        # Start animations for any animated GIFs
        self._start_animations()
        
        # Update status panel
        self.update_status_panel()
        
        # Force button colors after window is fully initialized (helps on macOS)
        self.after(100, self._force_button_colors)
    
    def _configure_styles(self):
        """Configure ttk styles for game theme"""
        # Configure Spinbox - light theme
        self.style.configure('TSpinbox',
                           fieldbackground=COLORS['grid_bg'],
                           background=COLORS['grid_bg'],
                           foreground=COLORS['text_dark'],
                           borderwidth=1)
        
        # Configure Combobox - light theme
        self.style.configure('TCombobox',
                           fieldbackground=COLORS['grid_bg'],
                           background=COLORS['grid_bg'],
                           foreground=COLORS['text_dark'],
                           borderwidth=1)
        
        # Configure Entry - light theme
        self.style.configure('TEntry',
                           fieldbackground=COLORS['grid_bg'],
                           foreground=COLORS['text_dark'],
                           borderwidth=1)
        
        # Configure Frame
        self.style.configure('TFrame',
                           background=COLORS['bg_panel'])
        
        # Configure Notebook tabs - light theme
        self.style.configure('TNotebook',
                           background=COLORS['bg_panel'],
                           borderwidth=0)
        self.style.configure('TNotebook.Tab',
                           background=COLORS['grid_bg'],
                           foreground=COLORS['text_dark'],
                           padding=[15, 10],
                           font=('Arial', 10, 'bold'))
        self.style.map('TNotebook.Tab',
                      background=[('selected', COLORS['bg_button']),
                                 ('active', COLORS['bg_button_hover'])],
                      foreground=[('selected', COLORS['text_light']),
                                 ('active', COLORS['text_light'])])
        self.style.configure('Game.TButton',
                           background=COLORS['bg_button'],
                           foreground=COLORS['text_light'],
                           borderwidth=0,
                           focuscolor='none',
                           font=('Arial', 10, 'bold'),
                           padding=10)
        self.style.map('Game.TButton',
                      background=[('active', COLORS['bg_button_hover']),
                                 ('pressed', COLORS['bg_button_active']),
                                 ('disabled', '#BDC3C7')],
                      foreground=[('active', COLORS['text_light']),
                                 ('pressed', COLORS['text_light']),
                                 ('disabled', '#ECF0F1')])
        
        # Configure Success Button (green)
        self.style.configure('Success.TButton',
                           background=COLORS['success'],
                           foreground=COLORS['text_light'],
                           borderwidth=0,
                           focuscolor='none',
                           font=('Arial', 10, 'bold'),
                           padding=10)
        self.style.map('Success.TButton',
                      background=[('active', '#40C057'),
                                 ('pressed', '#2F9E44')],
                      foreground=[('active', COLORS['text_light']),
                                 ('pressed', COLORS['text_light'])])
        
        # Configure Danger Button (red/coral)
        self.style.configure('Danger.TButton',
                           background=COLORS['danger'],
                           foreground=COLORS['text_light'],
                           borderwidth=0,
                           focuscolor='none',
                           font=('Arial', 10, 'bold'),
                           padding=10)
        self.style.map('Danger.TButton',
                      background=[('active', '#FF5252'),
                                 ('pressed', '#E53935')],
                      foreground=[('active', COLORS['text_light']),
                                 ('pressed', COLORS['text_light'])])
        
        # Configure Warning Button (yellow)
        self.style.configure('Warning.TButton',
                           background=COLORS['warning'],
                           foreground=COLORS['text_dark'],
                           borderwidth=0,
                           focuscolor='none',
                           font=('Arial', 10, 'bold'),
                           padding=10)
        self.style.map('Warning.TButton',
                      background=[('active', '#FFE066'),
                                 ('pressed', '#FFD700')],
                      foreground=[('active', COLORS['text_dark']),
                                 ('pressed', COLORS['text_dark'])])

    def _create_modern_card(self, parent, radius=15, shadow_offset=4, bg=COLORS['grid_bg']):
        """Create a modern card container with clean borders and shadow effect"""
        # Create outer frame for shadow/padding
        shadow_frame = tk.Frame(parent, bg=COLORS['bg_light'])
        
        # Create a subtle shadow frame (darker background showing through)
        shadow_bg_frame = tk.Frame(shadow_frame, bg='#E8E8E8', relief=tk.FLAT, bd=0)
        shadow_bg_frame.pack(fill=tk.BOTH, expand=True, padx=shadow_offset, pady=shadow_offset)
        
        # Create content frame with clean border
        content_frame = tk.Frame(shadow_bg_frame, bg=bg, relief=tk.FLAT, bd=0,
                                highlightbackground=COLORS['border'], 
                                highlightthickness=1)
        content_frame.pack(fill=tk.BOTH, expand=True, padx=1, pady=1)
        
        return shadow_frame, content_frame, None

    def _create_rounded_badge(self, parent, text, bg_color, fg_color=COLORS['text_light'], 
                              radius=20, padx=15, pady=8):
        """Create a modern rounded badge with shadow - returns update function"""
        badge_frame = tk.Frame(parent, bg=parent.cget('bg') if hasattr(parent, 'cget') else COLORS['bg_panel'])
        
        canvas = tk.Canvas(badge_frame, highlightthickness=0, 
                          bg=badge_frame.cget('bg'), height=35)
        canvas.pack()
        
        def draw_badge(txt=text, bg=bg_color, fg=fg_color):
            canvas.delete("all")
            width = canvas.winfo_width()
            height = 35
            
            if width > 1:
                # Draw shadow
                self._draw_rounded_rectangle(canvas, 2, 2, width-2, height-2,
                                           radius=radius, fill='#E0E0E0', outline='', width=0)
                # Draw badge
                self._draw_rounded_rectangle(canvas, 0, 0, width-4, height-4,
                                           radius=radius, fill=bg, outline=bg, width=0)
                
                # Draw text with bold font (slightly larger)
                font_tuple = MODERN_FONTS['body']
                # Increase font size from 11 to 13 for better visibility
                bold_font = (font_tuple[0], 13, 'bold')
                canvas.create_text(width//2 - 2, height//2 - 2, text=txt,
                                 fill=fg, font=bold_font)
        
        def update_badge(new_text, new_bg=None, new_fg=None):
            new_bg = new_bg if new_bg else bg_color
            new_fg = new_fg if new_fg else fg_color
            draw_badge(new_text, new_bg, new_fg)
        
        canvas.bind('<Configure>', lambda e: draw_badge())
        canvas.update_idletasks()
        draw_badge()
        
        # Return both frame and update function
        badge_frame.update_badge = update_badge
        return badge_frame

    def _build_header(self):
        """Build header with title and quick stats - modernized with rounded badges"""
        # Create header with white background card
        header_shadow, header_content, _ = self._create_modern_card(
            self.main_container, radius=12, shadow_offset=3, bg=COLORS['grid_bg'])
        header_shadow.pack(fill=tk.X, pady=(0, 15), padx=0)
        
        # Store reference to header card for color updates
        self.header_card = header_content
        
        # Configure header height
        header_shadow.configure(height=70)
        
        # Title with modern font
        title = tk.Label(header_content, text="WUMPUS WORLD", 
                        font=MODERN_FONTS['title'],
                        bg=COLORS['grid_bg'], fg=COLORS['info'])
        title.pack(side=tk.LEFT, padx=25, pady=15)
        
        # Store reference to title for color updates
        self.header_title = title
        
        # Quick status indicators with modern rounded badges
        self.status_indicators = tk.Frame(header_content, bg=COLORS['grid_bg'])
        self.status_indicators.pack(side=tk.RIGHT, padx=25, pady=15)
        
        # Status badge frame
        badge_frame = tk.Frame(self.status_indicators, bg=COLORS['grid_bg'])
        badge_frame.pack(side=tk.RIGHT)
        
        # Store reference to badge frame for color updates
        self.header_badge_frame = badge_frame
        
        # Create rounded badges
        self.alive_badge = self._create_rounded_badge(
            badge_frame, "Alive", COLORS['success'], COLORS['text_light'], radius=18)
        self.alive_badge.pack(side=tk.LEFT, padx=6)
        
        self.gold_badge = self._create_rounded_badge(
            badge_frame, "No Gold", COLORS['border'], COLORS['text_dark'], radius=18)
        self.gold_badge.pack(side=tk.LEFT, padx=6)

    def _build_status_panel(self):
        """Build right sidebar with game status and info - modernized with cards"""
        # Create modern white card container for status panel
        status_shadow, status_content, _ = self._create_modern_card(
            self.middle_container, radius=15, shadow_offset=4, bg=COLORS['grid_bg'])
        status_shadow.pack(side=tk.RIGHT, fill=tk.Y, padx=(10, 0))
        status_shadow.configure(width=280)
        
        # Title section with modern font
        title_section = tk.Frame(status_content, bg=COLORS['grid_bg'])
        title_section.pack(fill=tk.X, pady=(20, 15))
        
        status_title = tk.Label(title_section, text="📊 Game Status", 
                               font=MODERN_FONTS['heading'],
                               bg=COLORS['grid_bg'], fg=COLORS['text_dark'])
        status_title.pack(padx=20)
        
        # Status info frame
        info_frame = tk.Frame(status_content, bg=COLORS['grid_bg'])
        info_frame.pack(fill=tk.BOTH, expand=True, padx=15, pady=5)
        
        # Helper function to create modern status cards
        def create_status_card(parent, label_text, value_widget_callback):
            card_shadow, card_content, _ = self._create_modern_card(
                parent, radius=12, shadow_offset=3, bg=COLORS['grid_bg'])
            card_shadow.pack(fill=tk.X, pady=8, padx=5)
            card_shadow.configure(height=80)
            
            label = tk.Label(card_content, text=label_text, font=MODERN_FONTS['small'],
                            bg=COLORS['grid_bg'], fg=COLORS['text_muted'], anchor="w")
            label.pack(fill=tk.X, padx=12, pady=(12, 4))
            
            value_widget = value_widget_callback(card_content)
            value_widget.pack(fill=tk.X, padx=12, pady=(0, 12))
            
            return value_widget
        
        # Player position card
        self.pos_label = create_status_card(
            info_frame, "📍 Position",
            lambda p: tk.Label(p, text="(0, 0)", font=MODERN_FONTS['subheading'],
                             bg=COLORS['grid_bg'], fg=COLORS['info'], anchor="w"))
        
        # Current status card
        self.curr_status_label = create_status_card(
            info_frame, "🎯 Status",
            lambda p: tk.Label(p, text="Exploring", font=MODERN_FONTS['body'],
                             bg=COLORS['grid_bg'], fg=COLORS['info'], anchor="w"))
        
        # Perceptions card
        self.perception_text = create_status_card(
            info_frame, "👁️ Perceptions",
            lambda p: tk.Label(p, text="None", font=MODERN_FONTS['body'],
                             bg=COLORS['grid_bg'], fg=COLORS['text_dark'],
                             anchor="w", justify=tk.LEFT, wraplength=240))
        
        # Instructions section with modern card
        instructions_shadow, instructions_content, _ = self._create_modern_card(
            status_content, radius=12, shadow_offset=3, bg=COLORS['grid_bg'])
        instructions_shadow.pack(fill=tk.X, padx=15, pady=(10, 20))
        instructions_shadow.configure(height=100)
        
        instructions_title = tk.Label(instructions_content, text="⌨️ Controls", 
                                     font=MODERN_FONTS['subheading'],
                                     bg=COLORS['grid_bg'], fg=COLORS['text_dark'],
                                     anchor="w")
        instructions_title.pack(fill=tk.X, padx=12, pady=(12, 8))
        
        controls_text = tk.Label(instructions_content, 
                                text="WASD / Arrow Keys: Move\nSpace/G: Grab Gold\nC: Climb Out",
                                font=MODERN_FONTS['small'],
                                bg=COLORS['grid_bg'], fg=COLORS['text_muted'],
                                anchor="w", justify=tk.LEFT)
        controls_text.pack(fill=tk.X, padx=12, pady=(0, 12))

    def _force_button_colors(self):
        """Force all buttons to use correct colors - helps override system theme on macOS"""
        # With ttk.Button, colors are handled by styles, so this is less critical
        # But we'll keep it for any tk.Button instances that might exist
        def update_buttons_recursive(parent):
            for widget in parent.winfo_children():
                if isinstance(widget, tk.Button):
                    try:
                        current_bg = widget.cget('bg')
                        # If button is white or default system color, force our colors
                        if current_bg in ['#f0f0f0', '#ffffff', 'SystemButtonFace', 'white']:
                            widget.configure(bg=COLORS['bg_button'], fg=COLORS['text_light'])
                    except:
                        pass
                elif hasattr(widget, 'winfo_children'):
                    update_buttons_recursive(widget)
        
        update_buttons_recursive(self.main_container)

    def _create_button(self, parent, text, command, *args, **kwargs):
        """Create a styled button using ttk.Button for better macOS compatibility"""
        bg = kwargs.pop('bg', COLORS['bg_button'])
        width = kwargs.pop('width', None)
        height = kwargs.pop('height', None)
        
        # Determine button style based on background color
        if bg == COLORS['success']:
            style_name = 'Success.TButton'
        elif bg == COLORS['danger']:
            style_name = 'Danger.TButton'
        else:
            style_name = 'Game.TButton'
        
        btn = ttk.Button(parent, text=text, 
                        command=lambda: command(*args) if args else command(),
                        style=style_name,
                        width=width,
                        cursor="hand2")
        
        return btn

    def _draw_rounded_rectangle(self, canvas, x1, y1, x2, y2, radius=20, **kwargs):
        """Draw a rounded rectangle on a canvas using arcs and lines"""
        fill_color = kwargs.pop('fill', 'white')
        outline_color = kwargs.pop('outline', fill_color)
        width = kwargs.pop('width', 0)
        
        # Ensure radius doesn't exceed half the width or height
        radius = min(radius, abs(x2-x1)//2, abs(y2-y1)//2)
        
        # Draw the rounded rectangle
        # Top and bottom lines
        canvas.create_line(x1 + radius, y1, x2 - radius, y1, fill=outline_color, width=width)
        canvas.create_line(x1 + radius, y2, x2 - radius, y2, fill=outline_color, width=width)
        # Left and right lines
        canvas.create_line(x1, y1 + radius, x1, y2 - radius, fill=outline_color, width=width)
        canvas.create_line(x2, y1 + radius, x2, y2 - radius, fill=outline_color, width=width)
        
        # Draw corner arcs
        # Top-left
        canvas.create_arc(x1, y1, x1 + 2*radius, y1 + 2*radius, 
                         start=90, extent=90, fill=fill_color, outline=outline_color, width=width)
        # Top-right
        canvas.create_arc(x2 - 2*radius, y1, x2, y1 + 2*radius,
                         start=0, extent=90, fill=fill_color, outline=outline_color, width=width)
        # Bottom-left
        canvas.create_arc(x1, y2 - 2*radius, x1 + 2*radius, y2,
                         start=180, extent=90, fill=fill_color, outline=outline_color, width=width)
        # Bottom-right
        canvas.create_arc(x2 - 2*radius, y2 - 2*radius, x2, y2,
                         start=270, extent=90, fill=fill_color, outline=outline_color, width=width)
        
        # Fill the center rectangle
        canvas.create_rectangle(x1 + radius, y1, x2 - radius, y2, 
                                fill=fill_color, outline='', width=0)
        canvas.create_rectangle(x1, y1 + radius, x2, y2 - radius,
                                fill=fill_color, outline='', width=0)

    def _create_rounded_button(self, parent, text, command, width=150, height=40, 
                               bg=COLORS['success'], fg=COLORS['text_light'], 
                               font=("Segoe UI", 11, "bold"), radius=20):
        """Create a custom rounded button using Canvas"""
        # Modern font fallback - use system font if custom font not available
        modern_fonts = ["Segoe UI", "Helvetica Neue", "SF Pro Display", "San Francisco", "Arial"]
        button_font = font
        
        # Use the font provided or fallback to Arial
        if isinstance(font, tuple):
            button_font = font
        else:
            button_font = (modern_fonts[-1], 11, "bold")  # Fallback to Arial
        
        # Create frame for button
        btn_frame = tk.Frame(parent, bg=parent.cget('bg') if hasattr(parent, 'cget') else COLORS['bg_light'])
        
        # Create canvas for rounded button
        canvas = tk.Canvas(btn_frame, width=width, height=height, 
                          highlightthickness=0, bg=btn_frame.cget('bg'))
        canvas.pack()
        
        # Draw rounded rectangle background
        self._draw_rounded_rectangle(canvas, 0, 0, width, height, radius=radius,
                                     fill=bg, outline=bg, width=0)
        
        # Add hover effect
        def on_enter(e):
            canvas.delete("all")
            # Use darker shade for hover - if it's success color, use darker green
            if bg == COLORS.get('success', '#51CF66'):
                hover_bg = '#45B85A'  # Darker green
            else:
                hover_bg = COLORS.get('bg_button_hover', '#5BA0F2')
            self._draw_rounded_rectangle(canvas, 0, 0, width, height, radius=radius,
                                         fill=hover_bg, outline=hover_bg, width=0)
            canvas.create_text(width//2, height//2, text=text, fill=fg, 
                             font=button_font)
        
        def on_leave(e):
            canvas.delete("all")
            self._draw_rounded_rectangle(canvas, 0, 0, width, height, radius=radius,
                                         fill=bg, outline=bg, width=0)
            canvas.create_text(width//2, height//2, text=text, fill=fg, 
                             font=button_font)
        
        def on_click(e):
            command()
        
        canvas.bind("<Enter>", on_enter)
        canvas.bind("<Leave>", on_leave)
        canvas.bind("<Button-1>", on_click)
        canvas.config(cursor="hand2")
        
        # Draw text
        canvas.create_text(width//2, height//2, text=text, fill=fg, font=button_font)
        
        return btn_frame

    def _show_game_over_popup(self):
        """Show a game over popup with restart button - modern design with rounded corners"""
        if self.game_over_popup_shown:
            return  # Already shown, don't show again
        
        self.game_over_popup_shown = True
        
        # Modern font fallback list
        modern_fonts = ["Segoe UI", "Helvetica Neue", "SF Pro Display", "San Francisco", "Arial"]
        title_font = (modern_fonts[0], 28, "bold")  # Try Segoe UI first
        body_font = (modern_fonts[0], 13)  # Body text font
        
        # Create popup window
        popup = tk.Toplevel(self)
        popup.title("Game Over")
        popup.geometry("450x280")
        popup.configure(bg=COLORS['bg_light'])
        popup.resizable(False, False)
        
        # Center the popup on screen
        popup.update_idletasks()
        x = (popup.winfo_screenwidth() // 2) - (popup.winfo_width() // 2)
        y = (popup.winfo_screenheight() // 2) - (popup.winfo_height() // 2)
        popup.geometry(f"+{x}+{y}")
        
        # Make popup modal
        popup.transient(self)
        popup.grab_set()
        
        # Main container frame
        container_frame = tk.Frame(popup, bg=COLORS['bg_light'])
        container_frame.pack(fill=tk.BOTH, expand=True, padx=20, pady=20)
        
        # Create canvas for rounded background panel
        panel_width = 410
        panel_height = 240
        panel_radius = 25
        
        canvas = tk.Canvas(container_frame, width=panel_width, height=panel_height,
                          highlightthickness=0, bg=COLORS['bg_light'])
        canvas.pack()
        
        # Draw rounded rectangle background panel
        panel_bg = COLORS['grid_bg']  # White background for contrast
        self._draw_rounded_rectangle(canvas, 0, 0, panel_width, panel_height, 
                                     radius=panel_radius, fill=panel_bg, 
                                     outline=COLORS['border'], width=2)
        
        # Content frame (on top of canvas)
        content_frame = tk.Frame(canvas, bg=panel_bg)
        canvas.create_window(panel_width//2, panel_height//2, window=content_frame, anchor="center")
        
        # Game Over title
        title_label = tk.Label(content_frame, 
                              text="GAME OVER",
                              font=title_font,
                              bg=panel_bg,
                              fg=COLORS['danger'])
        title_label.pack(pady=(25, 15))
        
        # Message
        msg_label = tk.Label(content_frame,
                            text="You fell into a hole!",
                            font=body_font,
                            bg=panel_bg,
                            fg=COLORS['text_dark'])
        msg_label.pack(pady=(0, 30))
        
        # Button frame
        button_frame = tk.Frame(content_frame, bg=panel_bg)
        button_frame.pack()
        
        # Create rounded restart button
        restart_btn_frame = self._create_rounded_button(
            button_frame,
            text="Restart",
            command=lambda: self._on_restart(popup),
            width=180,
            height=50,
            bg=COLORS['success'],
            fg=COLORS['text_light'],
            font=(modern_fonts[0], 13, "bold"),
            radius=25
        )
        restart_btn_frame.pack(pady=10)
        
        # Update canvas to ensure proper display
        canvas.update_idletasks()
        
        # Focus on popup
        popup.focus_set()

    def _on_restart(self, popup):
        """Handle restart button click - reset game and close popup"""
        # Close popup
        popup.destroy()
        
        # Reset game over flag
        self.game_over_popup_shown = False
        
        # Stop any running simulation
        if self.running:
            self.running = False
        
        # Reset AI state
        self.ai_active = False
        
        # Reset the game using default setup
        self.on_default_setup()
        
        # Update message
        self.msg_var.set("Game restarted! Good luck!")

    def update_status_panel(self):
        """Update status panel with current game state"""
        if not hasattr(self, 'pos_label'):
            return
        
        # Update badges if they exist
        if hasattr(self, 'alive_badge'):
            if self.gs.alive:
                self.alive_badge.update_badge("❤️ Alive", COLORS['success'], COLORS['text_light'])
            else:
                self.alive_badge.update_badge("Dead", COLORS['danger'], COLORS['text_light'])
        
        if hasattr(self, 'gold_badge'):
            if self.gs.has_gold:
                self.gold_badge.update_badge("💰 Has Gold", COLORS['accent_gold'], COLORS['text_dark'])
            else:
                self.gold_badge.update_badge("❌ No Gold", COLORS['border'], COLORS['text_dark'])
            
        x, y = self.gs.player_pos
        if hasattr(self, 'pos_label'):
            self.pos_label.config(text=f"({x}, {y})")
        
        # Update current status card
        if hasattr(self, 'curr_status_label'):
            if self.gs.has_escaped:
                self.curr_status_label.config(text="🎉 Escaped!", fg=COLORS['success'])
            elif not self.gs.alive:
                self.curr_status_label.config(text="💀 Game Over", fg=COLORS['danger'])
            elif self.gs.has_gold:
                self.curr_status_label.config(text="Returning to Start", fg=COLORS['warning'])
            else:
                self.curr_status_label.config(text="Exploring", fg=COLORS['info'])
        
        # Update perceptions
        if hasattr(self, 'perception_text'):
            status = self.gs.get_self_status()
            perceptions = []
            if STAT_STENCH in status:
                perceptions.append("Stench")
            if STAT_WIND in status:
                perceptions.append("Wind")
            if STAT_GOLD in status:
                perceptions.append("Glitter")
            
            if perceptions:
                self.perception_text.config(text=", ".join(perceptions), fg=COLORS['warning'])
            else:
                self.perception_text.config(text="None", fg=COLORS['text_muted'])

    def _build_controls(self):
        """Build control panel at bottom - modernized with card design"""
        # Create modern white card for controls
        controls_shadow, controls_content, _ = self._create_modern_card(
            self.main_container, radius=15, shadow_offset=4, bg=COLORS['grid_bg'])
        controls_shadow.pack(fill=tk.X, pady=(15, 0))
        controls_shadow.configure(height=180)
        
        # Create notebook for tabs with modern styling
        notebook = ttk.Notebook(controls_content, style='TNotebook')
        notebook.pack(fill=tk.BOTH, expand=True, padx=15, pady=15)
        
        # Tab 1: Game Setup
        setup_tab = tk.Frame(notebook, bg=COLORS['grid_bg'])
        notebook.add(setup_tab, text="⚙️ Game Setup")
        
        setup_frame = tk.Frame(setup_tab, bg=COLORS['grid_bg'])
        setup_frame.pack(fill=tk.X, padx=15, pady=15)
        
        # Grid size section
        size_frame = tk.Frame(setup_frame, bg=COLORS['grid_bg'])
        size_frame.pack(side=tk.LEFT, padx=10)
        
        tk.Label(size_frame, text="Grid Size:", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['body']).pack(side=tk.LEFT, padx=5)
        tk.Label(size_frame, text="Rows:", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['small']).pack(side=tk.LEFT, padx=2)
        self.row_var = tk.IntVar(value=7)
        row_spin = ttk.Spinbox(size_frame, from_=3, to=11, textvariable=self.row_var, width=5)
        row_spin.pack(side=tk.LEFT, padx=2)
        
        tk.Label(size_frame, text="Cols:", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['small']).pack(side=tk.LEFT, padx=2)
        self.col_var = tk.IntVar(value=7)
        col_spin = ttk.Spinbox(size_frame, from_=3, to=11, textvariable=self.col_var, width=5)
        col_spin.pack(side=tk.LEFT, padx=2)
        
        self._create_button(setup_frame, "Resize", self.on_resize).pack(side=tk.LEFT, padx=5)
        self._create_button(setup_frame, "Default Setup", self.on_default_setup).pack(side=tk.LEFT, padx=5)
        self._create_button(setup_frame, "🎲 Randomize", self.on_randomize).pack(side=tk.LEFT, padx=5)
        
        # Tab 2: Manual Controls
        manual_tab = tk.Frame(notebook, bg=COLORS['grid_bg'])
        notebook.add(manual_tab, text="🎮 Manual Play")
        
        manual_frame = tk.Frame(manual_tab, bg=COLORS['grid_bg'])
        manual_frame.pack(fill=tk.X, padx=15, pady=15)
        
        # Movement controls
        move_frame = tk.Frame(manual_frame, bg=COLORS['grid_bg'])
        move_frame.pack(side=tk.LEFT, padx=15)
        
        tk.Label(move_frame, text="🎯 Movement:", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['body']).pack(pady=(0, 8))
        
        self.manual_frame = tk.Frame(move_frame, bg=COLORS['grid_bg'])
        self.manual_frame.pack(pady=5)
        
        # Movement buttons in cross pattern
        self._create_button(self.manual_frame, "↑", self.on_manual_move, "UP", width=4).grid(row=0, column=1, padx=3, pady=3)
        self._create_button(self.manual_frame, "←", self.on_manual_move, "LEFT", width=4).grid(row=1, column=0, padx=3, pady=3)
        self._create_button(self.manual_frame, "↓", self.on_manual_move, "DOWN", width=4).grid(row=1, column=1, padx=3, pady=3)
        self._create_button(self.manual_frame, "→", self.on_manual_move, "RIGHT", width=4).grid(row=1, column=2, padx=3, pady=3)
        
        # Actions
        action_frame = tk.Frame(manual_frame, bg=COLORS['grid_bg'])
        action_frame.pack(side=tk.LEFT, padx=15)
        
        tk.Label(action_frame, text="⚡ Actions:", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['body']).pack(pady=(0, 8))
        
        self._create_button(action_frame, "💰 Grab Gold", self.on_manual_grab).pack(pady=8, fill=tk.X)
        self._create_button(action_frame, "🚪 Climb Out", self.on_manual_climb).pack(pady=8, fill=tk.X)
        
        # Tab 3: AI Controls
        ai_tab = tk.Frame(notebook, bg=COLORS['grid_bg'])
        notebook.add(ai_tab, text="🤖 AI Mode")
        
        ai_frame = tk.Frame(ai_tab, bg=COLORS['grid_bg'])
        ai_frame.pack(fill=tk.X, padx=15, pady=15)
        
        # AI Mode selection
        mode_frame = tk.Frame(ai_frame, bg=COLORS['grid_bg'])
        mode_frame.pack(side=tk.LEFT, padx=8)
        
        tk.Label(mode_frame, text="AI Mode:", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['body']).pack(side=tk.LEFT, padx=3)
        self.ai_mode_var = tk.StringVar(value="Manual")
        ai_mode_combo = ttk.Combobox(mode_frame, textvariable=self.ai_mode_var, width=15,
                                     values=["Manual", "Prolog AI"], state='readonly')
        ai_mode_combo.pack(side=tk.LEFT, padx=3)
        self._create_button(mode_frame, "Activate AI", self.on_activate_ai).pack(side=tk.LEFT, padx=5)
        
        # Algorithm selection
        algo_frame = tk.Frame(ai_frame, bg=COLORS['grid_bg'])
        algo_frame.pack(side=tk.LEFT, padx=8)
        
        tk.Label(algo_frame, text="Algorithm:", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['body']).pack(side=tk.LEFT, padx=3)
        self.algorithm_var = tk.StringVar(value="bfs")
        algorithm_combo = ttk.Combobox(algo_frame, textvariable=self.algorithm_var, width=10,
                                     values=["bfs", "dfs", "astar", "ids", "prm", "ucs"], state='readonly')
        algorithm_combo.pack(side=tk.LEFT, padx=3)
        
        self._create_button(algo_frame, "Compute Path", self.on_compute_path).pack(side=tk.LEFT, padx=5)
        self._create_button(algo_frame, "Show Path", self.on_show_path).pack(side=tk.LEFT, padx=5)
        self._create_button(algo_frame, "Clear Path", self.on_clear_path).pack(side=tk.LEFT, padx=5)
        
        # Simulation controls
        sim_frame = tk.Frame(ai_frame, bg=COLORS['grid_bg'])
        sim_frame.pack(side=tk.LEFT, padx=8)
        
        tk.Label(sim_frame, text="Speed (ms):", bg=COLORS['grid_bg'], 
                fg=COLORS['text_dark'], font=MODERN_FONTS['small']).pack(side=tk.LEFT, padx=3)
        self.period_entry = tk.Entry(sim_frame, width=6, bg=COLORS['grid_bg'], 
                                     fg=COLORS['text_dark'], 
                                     insertbackground=COLORS['text_dark'],
                                     relief=tk.FLAT, bd=1,
                                     highlightthickness=1,
                                     highlightbackground=COLORS['border'],
                                     highlightcolor=COLORS['info'])
        self.period_entry.insert(0, "1000")
        self.period_entry.pack(side=tk.LEFT, padx=3)
        
        self._create_button(sim_frame, "▶ Simulate", self.on_simulate, bg=COLORS['success']).pack(side=tk.LEFT, padx=5)
        self._create_button(sim_frame, "⏹ Abort", self.on_abort, bg=COLORS['danger']).pack(side=tk.LEFT, padx=5)
        self._create_button(sim_frame, "⏭ Step", self.on_step).pack(side=tk.LEFT, padx=5)
        
        # Message bar at bottom with modern styling and rounded corners
        msg_shadow, msg_content, _ = self._create_modern_card(
            controls_content, radius=10, shadow_offset=2, bg=COLORS['grid_bg'])
        msg_shadow.pack(fill=tk.X, padx=15, pady=(0, 15))
        msg_shadow.configure(height=45)
        
        self.msg_var = tk.StringVar(value="Welcome! Use WASD keys or buttons to move. Find the gold and escape!")
        msg_label = tk.Label(msg_content, textvariable=self.msg_var, 
                           bg=COLORS['grid_bg'], fg=COLORS['text_dark'],
                           font=MODERN_FONTS['small'], anchor="w", padx=15, pady=12)
        msg_label.pack(fill=tk.BOTH, expand=True)
        
        # Store reference to bottom frame for compatibility
        self.bottom = controls_content
        
        # Initially enable manual controls
        self.update_manual_controls_state()

    def _build_grid(self):
        for child in self.center.winfo_children():
            child.destroy()

        self.buttons = []

        # 1) Make grid cells square & equal sized
        for c in range(self.gs.width):
            self.center.grid_columnconfigure(c, weight=1, uniform="grid")
        for r in range(self.gs.height):
            self.center.grid_rowconfigure(r, weight=1, uniform="grid")

        # 2) Create buttons that FILL their grid cell with modern light theme styling
        for r in range(self.gs.height):
            row_btns = []
            for c in range(self.gs.width):
                b = tk.Button(
                    self.center,
                    bg=COLORS['cell_bg'],
                    fg=COLORS['text_dark'],
                    borderwidth=1,
                    relief=tk.FLAT,
                    highlightthickness=0,
                    activebackground=COLORS['bg_button_hover']
                )
                b.grid(row=r, column=c, padx=2, pady=2, sticky="nsew")
                row_btns.append(b)
            self.buttons.append(row_btns)

        # Bind resize to reload images at new size
        self.center.bind("<Configure>", lambda e: self._color_grid())




    def _init_images(self):
        """Initialize image paths and cache"""
        self.resource_dir = os.path.join(os.path.dirname(__file__), "resources")
        
        # Map entity types to image filenames
        self.image_files = {
            STAT_PLAYER: "stickman",  # Default fallback
            "PLAYER_UP": "player_up",
            "PLAYER_DOWN": "player_down", 
            "PLAYER_LEFT": "player_left",
            "PLAYER_RIGHT": "player_right",
            STAT_START: "start",
            STAT_WUMPUS: "wumpus",
            STAT_HOLE: "hole",
            STAT_GOLD: "gold",
            STAT_STENCH: "stench",
            STAT_WIND: "wind",
            "EMPTY": "empty",
        }
        
        # Cache for loaded base images (PIL Image or PhotoImage)
        self.base_images: Dict[str, any] = {}
        
        # Cache for scaled PhotoImages: (entity, width, height) -> PhotoImage
        self.scaled_images: Dict[Tuple[str, int, int], tk.PhotoImage] = {}
        
        # Animation support for GIFs
        self.animated_gifs: Dict[str, List[tk.PhotoImage]] = {}  # entity -> list of frames
        self.animation_frame_index: Dict[str, int] = {}  # entity -> current frame index
        self.animation_delays: Dict[str, int] = {}  # entity -> delay between frames in ms
        
        # Preload base images - try multiple extensions
        print(f"\n📂 Loading images from: {self.resource_dir}")
        print(f"PIL Available: {PIL_AVAILABLE}\n")
        
        for entity, base_filename in self.image_files.items():
            loaded = False
            for ext in [".png", ".jpg", ".jpeg", ".gif"]:
                path = os.path.join(self.resource_dir, base_filename + ext)
                if os.path.exists(path):
                    try:
                        if PIL_AVAILABLE:
                            img = Image.open(path)
                            
                            # Check if this is an animated GIF
                            if ext == ".gif" and hasattr(img, 'is_animated') and img.is_animated:
                                # Process animated GIF
                                self._load_animated_gif(entity, img, path)
                                print(f"✓ Loaded {entity:12s} from {base_filename}{ext:5s} (PIL - Animated GIF)")
                            else:
                                # Process static image
                                # Convert to RGBA for consistency
                                if img.mode not in ("RGBA", "RGB"):
                                    img = img.convert("RGBA")
                                elif img.mode == "RGB":
                                    img = img.convert("RGBA")
                                self.base_images[entity] = img
                                print(f"✓ Loaded {entity:12s} from {base_filename}{ext:5s} (PIL - {img.mode})")
                        else:
                            # Tkinter PhotoImage only supports GIF/PGM without PIL
                            if ext in [".gif"]:
                                self.base_images[entity] = tk.PhotoImage(file=path, master=self)
                                print(f"✓ Loaded {entity:12s} from {base_filename}{ext:5s} (Tkinter)")
                            else:
                                print(f"✗ Skipped {entity:12s} - {base_filename}{ext:5s} (need PIL for JPG/PNG)")
                        loaded = True
                        break
                    except Exception as e:
                        print(f"✗ Failed to load {base_filename}{ext}: {e}")
            
            if not loaded and entity != "EMPTY":
                print(f"⚠ Warning: No image found for {entity:12s} (tried {base_filename}.png/jpg/jpeg/gif)")
        print()

    def _load_animated_gif(self, entity: str, img: Image.Image, path: str):
        """Load and process an animated GIF into frames"""
        try:
            frames = []
            frame_count = 0
            max_frames = 10  # Limit frames for performance
            
            # Get original size for optimization
            original_width, original_height = img.size
            max_dimension = 64  # Performance optimization
            
            # Calculate resize ratio if needed
            if original_width > max_dimension or original_height > max_dimension:
                ratio = min(max_dimension / original_width, max_dimension / original_height)
                new_width = int(original_width * ratio)
                new_height = int(original_height * ratio)
                should_resize = True
                print(f"  📏 Resizing {entity} from {original_width}x{original_height} to {new_width}x{new_height}")
            else:
                should_resize = False
                new_width, new_height = original_width, original_height
            
            # Extract each frame
            try:
                while frame_count < max_frames:
                    img.seek(frame_count)
                    
                    # Get frame and convert to RGBA
                    frame = img.copy()
                    if frame.mode != "RGBA":
                        frame = frame.convert("RGBA")
                    
                    # Resize if needed
                    if should_resize:
                        frame = frame.resize((new_width, new_height), Image.Resampling.LANCZOS)
                    
                    # Convert to PhotoImage
                    photo = ImageTk.PhotoImage(frame, master=self)
                    frames.append(photo)
                    
                    frame_count += 1
                    
            except EOFError:
                # No more frames
                pass
            
            # Store the frames and animation info
            if len(frames) > 1:
                self.animated_gifs[entity] = frames
                self.animation_frame_index[entity] = 0
                
                # Get frame delay (default to 100ms if not available)
                try:
                    delay = img.info.get('duration', 100)
                    if delay < 50:  # Minimum delay for performance
                        delay = 100
                    self.animation_delays[entity] = delay
                except:
                    self.animation_delays[entity] = 100
                
                print(f"  🎬 Extracted {len(frames)} frames, delay: {self.animation_delays[entity]}ms")
            else:
                # Fallback to static image if only one frame
                if frames:
                    # Convert back to PIL image for consistency
                    self.base_images[entity] = frame
                print(f"  ⚠ Only {len(frames)} frame found, treating as static image")
                
        except Exception as e:
            print(f"  ✗ Error processing animated GIF for {entity}: {e}")
            # Fallback to loading as static image
            try:
                if img.mode != "RGBA":
                    img = img.convert("RGBA")
                self.base_images[entity] = img
                print(f"  📷 Fallback: loaded as static image")
            except Exception as e2:
                print(f"  ✗ Fallback also failed: {e2}")

    def _get_scaled_image(self, entity: str, width: int, height: int) -> Optional[tk.PhotoImage]:
        """Get a scaled PhotoImage for the given entity and size"""
        cache_key = (entity, width, height)
        
        # Return cached if available
        if cache_key in self.scaled_images:
            return self.scaled_images[cache_key]
        
        # Check if this is an animated GIF
        if entity in self.animated_gifs:
            current_frame_index = self.animation_frame_index.get(entity, 0)
            frames = self.animated_gifs[entity]
            if frames and current_frame_index < len(frames):
                base = frames[current_frame_index]
            else:
                base = self.base_images.get(entity)
        else:
            # Get base image for static images
            base = self.base_images.get(entity)
            
        if base is None:
            return None
        
        try:
            if PIL_AVAILABLE and isinstance(base, Image.Image):
                # Scale with PIL
                scaled = base.resize((width, height), Image.Resampling.LANCZOS)
                photo = ImageTk.PhotoImage(scaled, master=self)
            else:
                # Can't scale tkinter PhotoImage easily, use as-is
                photo = base
            
            # Don't cache animated frames to allow frame updates
            if entity not in self.animated_gifs:
                self.scaled_images[cache_key] = photo
            return photo
        except Exception as e:
            print(f"Error scaling image for {entity}: {e}")
            return None

    def _start_animations(self):
        """Start animation loops for any animated GIF entities"""
        try:
            # Initialize animation state for all animated entities
            for entity in self.animated_gifs:
                if entity not in self.animation_frame_index:
                    self.animation_frame_index[entity] = 0
                    
            # Start the animation update loop if we have any animations
            if self.animated_gifs:
                print(f"✓ Starting animations for {len(self.animated_gifs)} entities")
                self._update_animations()
            else:
                print("• No animated entities found")
                
        except Exception as e:
            print(f"Error starting animations: {e}")

    def _update_animations(self):
        """Update animation frames and schedule next update"""
        try:
            # Track if any animations actually updated
            updated = False
            
            # Update frame indices for all animated entities
            for entity in self.animated_gifs:
                if entity in self.animation_frame_index:
                    frames = self.animated_gifs[entity]
                    if len(frames) > 1:
                        # Advance to next frame
                        old_frame = self.animation_frame_index[entity]
                        self.animation_frame_index[entity] = (self.animation_frame_index[entity] + 1) % len(frames)
                        if old_frame != self.animation_frame_index[entity]:
                            updated = True
            
            # Only refresh grid if animations actually changed
            if updated:
                self._update_animated_cells()
            
            # Use slower update rate for better performance
            update_delay = 150  # 150ms = ~6.7 FPS, good balance of smooth vs performance
            if self.animation_delays:
                update_delay = max(150, min(self.animation_delays.values()))
            
            # Schedule next update
            self.after(update_delay, self._update_animations)
            
        except Exception as e:
            print(f"Animation update error: {e}")
            # Try again after a longer delay if there's an error
            self.after(500, self._update_animations)

    def _update_animated_cells(self):
        """Update only cells that contain animated entities (more efficient than full grid refresh)"""
        try:
            for r in range(self.gs.height):
                for c in range(self.gs.width):
                    cell = self.gs.grid[r][c]
                    # Check if this cell contains any animated entities
                    if STAT_WUMPUS in cell.contents and STAT_WUMPUS in self.animated_gifs:
                        # Recolor just this cell
                        self._color_single_cell(r, c)
        except Exception as e:
            print(f"Animated cell update error: {e}")
            # Fallback to full grid refresh
            self._color_grid()

    def _color_single_cell(self, r: int, c: int):
        """Update display for a single cell"""
        cell = self.gs.grid[r][c]
        
        # Same priority logic as _color_grid
        if STAT_PLAYER in cell.contents:
            entity = f"PLAYER_{self.player_direction}"
        elif STAT_GOLD in cell.contents:
            entity = STAT_GOLD
        elif STAT_WUMPUS in cell.contents:
            entity = STAT_WUMPUS
        elif STAT_HOLE in cell.contents:
            entity = STAT_HOLE
        elif STAT_START in cell.contents:
            entity = STAT_START
        elif STAT_STENCH in cell.contents:
            entity = STAT_STENCH
        elif STAT_WIND in cell.contents:
            entity = STAT_WIND
        else:
            entity = "EMPTY"
        
        # Get current cell size (approximate)
        try:
            grid_width = max(100, min(400, self.center.winfo_width()))
            grid_height = max(100, min(400, self.center.winfo_height()))
            cell_width = max(32, min(100, int(grid_width / max(1, self.gs.width)) - 4))
            cell_height = max(32, min(100, int(grid_height / max(1, self.gs.height)) - 4))
        except:
            cell_width, cell_height = 64, 64  # Fallback
        
        # Get scaled image and update button
        img = self._get_scaled_image(entity, cell_width, cell_height)
        
        # Fallback colors with modern light theme
        bg = COLORS['cell_bg']
        if entity == STAT_HOLE:
            bg = '#B0BEC5'  # Light gray-blue for pits
        elif entity == STAT_WUMPUS:
            bg = '#FFCDD2'  # Light red/pink for wumpus
        elif entity == STAT_START:
            bg = COLORS['success']  # Green for start
        elif entity == STAT_STENCH:
            bg = '#FFF9C4'  # Light yellow for stench
        elif entity == STAT_WIND:
            bg = '#B3E5FC'  # Light blue for wind
        
        # Player cells get raised border
        if STAT_PLAYER in cell.contents:
            relief = tk.RAISED
            bg = COLORS['info']  # Blue highlight for player
        else:
            relief = tk.FLAT
        
        if img:
            try:
                self.buttons[r][c]._img_ref = img
                self.buttons[r][c].configure(image=img, text="", bg=bg, relief=relief, compound="center")
            except tk.TclError:
                self.buttons[r][c].configure(image="", text="", bg=bg, relief=relief)
        else:
            self.buttons[r][c].configure(image="", text="", bg=bg, relief=relief)

    def _color_grid(self):
        """Update grid display with images based on cell contents"""
        # Calculate current cell size - use conservative sizing to prevent growth
        try:
            grid_width = self.center.winfo_width()
            grid_height = self.center.winfo_height()
        except:
            grid_width = 400  # fallback size
            grid_height = 400
            
        # Ensure minimum sizes and prevent excessive scaling
        grid_width = max(100, min(400, grid_width))
        grid_height = max(100, min(400, grid_height))
        
        cell_width = max(32, min(100, int(grid_width / max(1, self.gs.width)) - 4))
        cell_height = max(32, min(100, int(grid_height / max(1, self.gs.height)) - 4))
        
        for r in range(self.gs.height):
            for c in range(self.gs.width):
                cell = self.gs.grid[r][c]
                
                # Priority order for what image to show (highest priority first)
                if STAT_PLAYER in cell.contents:
                    # Use directional player image based on current direction
                    entity = f"PLAYER_{self.player_direction}"
                elif STAT_GOLD in cell.contents:
                    entity = STAT_GOLD
                elif STAT_WUMPUS in cell.contents:
                    entity = STAT_WUMPUS
                elif STAT_HOLE in cell.contents:
                    entity = STAT_HOLE
                elif STAT_START in cell.contents:
                    entity = STAT_START
                elif STAT_STENCH in cell.contents:
                    entity = STAT_STENCH
                elif STAT_WIND in cell.contents:
                    entity = STAT_WIND
                else:
                    entity = "EMPTY"
                
                # Get scaled image
                img = self._get_scaled_image(entity, cell_width, cell_height)
                
                # Fallback colors with modern light theme
                bg = COLORS['cell_bg']
                if entity == STAT_HOLE:
                    bg = '#B0BEC5'  # Light gray-blue for pits
                elif entity == STAT_WUMPUS:
                    bg = '#FFCDD2'  # Light red/pink for wumpus
                elif entity == STAT_START:
                    bg = COLORS['success']  # Green for start
                elif entity == STAT_STENCH:
                    bg = '#FFF9C4'  # Light yellow for stench
                elif entity == STAT_WIND:
                    bg = '#B3E5FC'  # Light blue for wind
                
                # Player cells get raised border
                if STAT_PLAYER in cell.contents:
                    relief = tk.RAISED
                    bg = COLORS['info']  # Blue highlight for player
                else:
                    relief = tk.FLAT
                
                if img:
                    try:
                        # Keep reference to prevent garbage collection BEFORE configuring
                        self.buttons[r][c]._img_ref = img
                        self.buttons[r][c].configure(image=img, text="", bg=bg, relief=relief, compound="center")
                    except tk.TclError as e:
                        print(f"Image error for {entity} at ({r},{c}): {e}")
                        # Fallback to color only
                        self.buttons[r][c].configure(image="", text="", bg=bg, relief=relief)
                else:
                    # No image, use color fallback
                    self.buttons[r][c].configure(image="", text="", bg=bg, relief=relief)

    # --- Control handlers ----------------------------------------------------
    def on_resize(self):
        if self.running:
            self.msg_var.set("Stop the game before resizing")
            return
        rows = max(3, min(11, int(self.row_var.get())))
        cols = max(3, min(11, int(self.col_var.get())))
        self.gs.reset(cols, rows)
        self._build_grid()
        self._color_grid()
        self.update_status_panel()
        self.msg_var.set(f"Resized to {cols}x{rows}")

    def on_default_setup(self):
        if self.running:
            self.msg_var.set("Stop the game before setup")
            return
        self.game_over_popup_shown = False  # Reset popup flag
        self.gs.setup_default_world()
        self.player_direction = "DOWN"  # Reset player direction to default
        self._color_grid()
        self.update_status_panel()
        
        # Clear any existing AI path
        if self.prolog_communicator:
            self.prolog_communicator.path_cache = []
            self.prolog_communicator.current_step = 0
        
        self.msg_var.set("Default world: Wumpus(3,4), Gold(6,2), Pits(2,3),(4,1),(1,5)")

    def on_randomize(self):
        if self.running:
            self.msg_var.set("Stop the game before randomize")
            return
        self.game_over_popup_shown = False  # Reset popup flag
        self.gs.randomize(wumpi=1, holes=1)
        self.player_direction = "DOWN"  # Reset player direction to default
        self._color_grid()
        self.update_status_panel()
        
        # Clear any existing AI path
        if self.prolog_communicator:
            self.prolog_communicator.path_cache = []
            self.prolog_communicator.current_step = 0
            
        self.update_manual_controls_state()  # Update control states
            
        if self.ai_active:
            self.msg_var.set("New world generated! Select algorithm and compute path to begin.")
        else:
            self.msg_var.set("New world generated! Use movement buttons, WASD keys, or click actions.")

    def on_activate_ai(self):
        if self.running:
            self.msg_var.set("Stop simulation before changing AI mode")
            return
            
        ai_mode = self.ai_mode_var.get()
        
        if ai_mode == "Manual":
            self.ai_active = False
            self.prolog_communicator = None
            self.update_manual_controls_state()
            self.msg_var.set("Manual mode activated - use movement buttons, WASD keys, or click actions")
            
        elif ai_mode == "Prolog AI":
            if not PROLOG_AVAILABLE:
                self.msg_var.set("Prolog bridge not available")
                return
            
            prolog_file = os.path.join(os.path.dirname(__file__), "logic.pl")
            if not os.path.exists(prolog_file):
                self.msg_var.set("logic.pl not found")
                return
                
            # Only activate AI during actual simulation, not just for path computation
            self.ai_active = False  # Changed: Don't activate AI mode just for computing paths
            self.prolog_communicator = PrologCommunicator(
                "prolog_communication.txt", 
                self.gs.width, 
                self.gs.height, 
                prolog_file
            )
            self.update_manual_controls_state()
            self.msg_var.set("Prolog AI ready - manual controls remain active. Use 'Compute Path' then 'Simulate'")

    def on_simulate(self):
        if not self.prolog_communicator:
            self.msg_var.set("Activate Prolog AI mode first")
            return
        try:
            self.period_ms = max(50, int(self.period_entry.get()))
        except ValueError:
            self.period_ms = 1000
        if self.running:
            self.msg_var.set("Already running")
            return
        
        # Compute path first if not already done
        if not hasattr(self.prolog_communicator, 'path_cache') or not self.prolog_communicator.path_cache:
            algorithm = self.algorithm_var.get()
            success = self.prolog_communicator.compute_path(self.gs, algorithm)
            if not success:
                self.msg_var.set(f"No path found using {algorithm.upper()}")
                return
        
        # Reset step counter for fresh simulation
        self.prolog_communicator.current_step = 0
        
        self.running = True
        self.ai_active = True  # Temporarily activate AI mode for simulation
        self.update_manual_controls_state()  # Disable manual controls during simulation
        self.msg_var.set(f"AI simulation started - following {len(self.prolog_communicator.path_cache)} step path")
        threading.Thread(target=self._run_loop, daemon=True).start()

    def on_abort(self):
        self.running = False
        # Restore manual mode if AI mode is "Manual"
        if self.ai_mode_var.get() == "Manual":
            self.ai_active = False
        self.gs.reset(self.gs.width, self.gs.height)
        self.player_direction = "DOWN"  # Reset player direction to default
        self._build_grid()
        self._color_grid()
        self.update_status_panel()
        self.update_manual_controls_state()  # Re-enable manual controls if in manual mode
        self.msg_var.set("Game reset - manual controls restored")

    def on_step(self):
        if self.ai_active and self.prolog_communicator:
            self._tick_once()
        else:
            self.msg_var.set("Activate AI mode first or use manual controls")

    def on_compute_path(self):
        if not self.prolog_communicator:
            self.msg_var.set("Activate Prolog AI mode first")
            return
        
        algorithm = self.algorithm_var.get()
        self.msg_var.set(f"Computing path using {algorithm.upper()}...")
        
        success = self.prolog_communicator.compute_path(self.gs, algorithm)
        
        if success:
            path_length = len(self.prolog_communicator.path_cache)
            self.msg_var.set(f"Path computed using {algorithm.upper()} - {path_length} steps. Manual controls still active.")
        else:
            self.msg_var.set(f"No path found using {algorithm.upper()}")

    def on_show_path(self):
        if not self.prolog_communicator:
            self.msg_var.set("Activate Prolog AI first")
            return
        
        if hasattr(self.prolog_communicator, 'path_cache') and self.prolog_communicator.path_cache:
            path_str = " → ".join([f"({x},{y})" for x, y in self.prolog_communicator.path_cache])
            self.msg_var.set(f"Path ({len(self.prolog_communicator.path_cache)} steps): {path_str}")
        else:
            self.msg_var.set("No path computed yet")

    def on_clear_path(self):
        if self.prolog_communicator and hasattr(self.prolog_communicator, 'path_cache'):
            self.prolog_communicator.path_cache = []
            self.prolog_communicator.current_step = 0
            self.msg_var.set("Path cleared")
        else:
            self.msg_var.set("No path to clear")

    def update_manual_controls_state(self):
        """Enable/disable manual controls based on current mode - ensure buttons remain visible"""
        manual_mode = not self.ai_active and not self.running
        
        # Enable/disable movement buttons (ttk.Button uses state parameter)
        for widget in self.manual_frame.winfo_children():
            if isinstance(widget, (tk.Button, ttk.Button)):
                widget.configure(state="normal" if manual_mode else "disabled")
        
        # Find and update action buttons - search recursively
        def update_buttons_recursive(parent):
            for widget in parent.winfo_children():
                if isinstance(widget, (tk.Button, ttk.Button)):
                    button_text = widget.cget("text")
                    if button_text in ["💰 Grab Gold", "🚪 Climb Out"]:
                        widget.configure(state="normal" if manual_mode else "disabled")
                elif hasattr(widget, 'winfo_children'):
                    update_buttons_recursive(widget)
        
        update_buttons_recursive(self.bottom)

    def on_manual_move(self, direction):
        """Handle manual movement button clicks"""
        if self.running or self.ai_active:
            self.msg_var.set("Cannot move manually during AI simulation")
            return
            
        old_pos = self.gs.player_pos
        # Update player direction before moving
        self.player_direction = direction
        self.gs.move_player(direction)
        self._color_grid()
        self.update_status_panel()
        
        if not self.gs.alive:
            self.msg_var.set("💀 You died!")
            self._show_game_over_popup()
        elif self.gs.has_gold:
            self.msg_var.set(f"Position: {self.gs.player_pos} | Has gold! Return to (0,0) and click 'Climb Out'")
        else:
            self.msg_var.set(f"Position: {self.gs.player_pos} | Use movement buttons or WASD keys")

    def on_manual_grab(self):
        """Handle manual gold pickup"""
        if self.running or self.ai_active:
            self.msg_var.set("Cannot grab gold during AI simulation")
            return
            
        old_has_gold = self.gs.has_gold
        self.gs.pickup()
        self.update_status_panel()
        
        if self.gs.has_gold and not old_has_gold:
            self.msg_var.set("Gold picked up! Return to start (0,0) and click 'Climb Out'")
            self._color_grid()
        elif self.gs.has_gold:
            self.msg_var.set("Already have the gold! Return to (0,0) and click 'Climb Out'")
        else:
            self.msg_var.set("No gold here to pick up")

    def on_manual_climb(self):
        """Handle manual climb out"""
        if self.running or self.ai_active:
            self.msg_var.set("Cannot climb during AI simulation")
            return
            
        if self.gs.player_pos != (0, 0):
            self.msg_var.set("Must be at start position (0,0) to climb out")
            return
            
        if not self.gs.has_gold:
            self.msg_var.set("Need gold to escape! Find the yellow square first")
            return
            
        self.gs.climb()
        self.update_status_panel()
        if self.gs.has_escaped:
            self.msg_var.set("🎉 Success! You escaped with the gold!")
            self._color_grid()

    def on_key_press(self, event):
        """Handle keyboard input for manual control"""
        if self.running or self.ai_active:
            return  # Don't allow manual control during AI simulation
            
        key = event.keysym.lower()
        moved = False
        
        if key in ['up', 'w']:
            self.player_direction = MOVE_UP
            self.gs.move_player(MOVE_UP)
            moved = True
        elif key in ['down', 's']:
            self.player_direction = MOVE_DOWN
            self.gs.move_player(MOVE_DOWN)
            moved = True
        elif key in ['left', 'a']:
            self.player_direction = MOVE_LEFT
            self.gs.move_player(MOVE_LEFT)
            moved = True
        elif key in ['right', 'd']:
            self.player_direction = MOVE_RIGHT
            self.gs.move_player(MOVE_RIGHT)
            moved = True
        elif key in ['space', 'g']:
            self.gs.pickup()
            self.update_status_panel()
            if self.gs.has_gold:
                self.msg_var.set("Gold picked up! Return to start (0,0) and press 'c' to climb out.")
                self._color_grid()
        elif key in ['c']:
            self.gs.climb()
            self.update_status_panel()
            if self.gs.has_escaped:
                self.msg_var.set("🎉 Success! You escaped with the gold!")
                self._color_grid()
        
        if moved:
            self._color_grid()
            self.update_status_panel()
            if not self.gs.alive:
                self.msg_var.set("💀 You died!")
                self._show_game_over_popup()
            elif self.gs.has_gold:
                self.msg_var.set(f"Position: {self.gs.player_pos} | Has gold! Return to (0,0) and press 'c'")
            else:
                self.msg_var.set(f"Position: {self.gs.player_pos} | Use WASD/Arrow keys to move, Space/G to grab gold")

    # --- Simulation loop -----------------------------------------------------
    def _run_loop(self):
        while self.running:
            self._tick_once()
            for _ in range(int(self.period_ms / 50)):
                if not self.running:
                    return
                time.sleep(0.05)

    def _tick_once(self):
        if not self.ai_active or not self.prolog_communicator:
            return
        
        # 1) Send perception
        self.prolog_communicator.send_perception(self.gs)
        # 2) Read command
        cmd = self.prolog_communicator.read_command(timeout_ms=1000)
        if cmd:
            self._apply_command(cmd)
        else:
            # No more commands - simulation complete
            self.running = False
            # Restore manual mode if AI mode is "Manual"
            if self.ai_mode_var.get() == "Manual":
                self.ai_active = False
            self.update_manual_controls_state()  # Re-enable controls if in manual mode
            self.update_status_panel()
            if self.gs.has_escaped:
                self.msg_var.set("🎉 Success! Gold collected and escaped! Manual controls restored.")
            elif self.gs.has_gold and self.gs.player_pos == (0, 0):
                self.msg_var.set("🎉 Success! Returned to start with gold! Manual controls restored.")
            else:
                self.msg_var.set("Simulation complete - manual controls restored")
            return
            
        # 3) Refresh UI
        self._color_grid()
        self.update_status_panel()
        
        # Check if simulation is complete
        if self.gs.has_escaped:
            self.running = False
            self.update_manual_controls_state()
            self.update_status_panel()
            self.msg_var.set("🎉 Success! Gold collected and escaped!")
        elif not self.gs.alive:
            self.running = False
            self.update_manual_controls_state()
            self.update_status_panel()
            self.msg_var.set("💀 Game Over! Player died.")
            self._show_game_over_popup()

    def _apply_command(self, cmd: str):
        # Expected format: B;INSTRUCTION;Scream;Pickup;Climb
        # e.g., B;UP;false;false;false
        parts = cmd.strip().split(";")
        if len(parts) < 5 or parts[0] != "B":
            return
        instr = parts[1]
        scream = parts[2].lower() == "true"  # currently ignored
        pickup = parts[3].lower() == "true"
        climb = parts[4].lower() == "true"
        
        # Handle pickup BEFORE movement to ensure we're at the right position
        if pickup:
            old_has_gold = self.gs.has_gold
            self.gs.pickup()
            if self.gs.has_gold and not old_has_gold:
                self.msg_var.set("Gold collected! Returning to start...")
            
        # Handle movement AFTER pickup (but skip movement if we just picked up gold)
        if instr in {MOVE_UP, MOVE_DOWN, MOVE_LEFT, MOVE_RIGHT}:
            # Don't move if we just picked up gold - stay at gold position
            if pickup and self.gs.has_gold:
                self.player_direction = instr  # Update direction but don't move
            else:
                self.player_direction = instr
                self.gs.move_player(instr)
                
        if climb:
            self.gs.climb()
            if self.gs.has_escaped:
                self.msg_var.set("🎉 Successfully escaped with the gold!")
        
        self.update_status_panel()


if __name__ == "__main__":
    app = WumpusGUI()
    app.mainloop()
