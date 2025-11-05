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

class WumpusGUI(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Treasure Hunt")
        self.geometry("600x600")  # Reduced from 800x800 for smaller grid
        self.resizable(True, True)

        self.gs = GameState(7, 7)
        self.gs.setup_default_world()  # Set up default world instead of empty
        self.buttons: List[List[tk.Button]] = []

        # Initialize instance variables BEFORE building controls
        self.prolog_communicator: Optional[PrologCommunicator] = None
        self.running = False
        self.period_ms = 1000
        self.ai_active = False
        self.player_direction = "DOWN"  # Default player facing direction

        self.bottom = tk.Frame(self)
        self.bottom.pack(side=tk.BOTTOM, fill=tk.X)
        
        # Create a container frame to center the grid
        self.container = tk.Frame(self)
        self.container.pack(side=tk.TOP, expand=True, fill=tk.BOTH)
        
        # Center frame for the grid - use pack for stable positioning
        self.center = tk.Frame(self.container)
        self.center.pack(expand=True)

        # Initialize image loader
        self._init_images()

        self._build_controls()
        self._build_grid()
        self._color_grid()
        
        # Add keyboard bindings for manual control
        self.bind("<Key>", self.on_key_press)
        self.focus_set()
        
        # Start animations for any animated GIFs
        self._start_animations()

    def _build_controls(self):
        frm = self.bottom

        tk.Label(frm, text="Rows").grid(row=0, column=0, padx=4)
        self.row_var = tk.IntVar(value=7)
        tk.Spinbox(frm, from_=3, to=11, textvariable=self.row_var, width=5).grid(row=0, column=1)

        tk.Label(frm, text="Cols").grid(row=0, column=2, padx=4)
        self.col_var = tk.IntVar(value=7)
        tk.Spinbox(frm, from_=3, to=11, textvariable=self.col_var, width=5).grid(row=0, column=3)

        tk.Button(frm, text="Resize", command=self.on_resize).grid(row=0, column=4, padx=6)
        tk.Button(frm, text="Default Setup", command=self.on_default_setup).grid(row=0, column=5, padx=6)
        tk.Button(frm, text="Randomize", command=self.on_randomize).grid(row=0, column=6, padx=6)

        tk.Label(frm, text="AI Mode").grid(row=0, column=7, padx=6)
        self.ai_mode_var = tk.StringVar(value="Manual")
        ai_mode_combo = ttk.Combobox(frm, textvariable=self.ai_mode_var, width=15,
                                   values=["Manual", "Prolog AI"])
        ai_mode_combo.grid(row=0, column=8, padx=4)
        ai_mode_combo.state(['readonly'])
        tk.Button(frm, text="Activate AI", command=self.on_activate_ai).grid(row=0, column=9, padx=6)

        tk.Label(frm, text="Period (ms)").grid(row=0, column=11, padx=6)
        self.period_entry = tk.Entry(frm, width=6)
        self.period_entry.insert(0, "1000")
        self.period_entry.grid(row=0, column=12)

        tk.Button(frm, text="Simulate", command=self.on_simulate).grid(row=0, column=13, padx=6)
        tk.Button(frm, text="Abort", command=self.on_abort).grid(row=0, column=14, padx=6)
        tk.Button(frm, text="Step", command=self.on_step).grid(row=0, column=15, padx=6)

        # Second row for Prolog controls
        tk.Label(frm, text="Algorithm").grid(row=1, column=0, padx=4)
        self.algorithm_var = tk.StringVar(value="bfs")
        algorithm_combo = ttk.Combobox(frm, textvariable=self.algorithm_var, width=8,
                                     values=["bfs", "dfs", "astar", "ids", "prm", "ucs"])
        algorithm_combo.grid(row=1, column=1, padx=4)
        algorithm_combo.state(['readonly'])

        tk.Button(frm, text="Compute Path", command=self.on_compute_path).grid(row=1, column=2, padx=6)
        tk.Button(frm, text="Show Path", command=self.on_show_path).grid(row=1, column=3, padx=6)
        tk.Button(frm, text="Clear Path", command=self.on_clear_path).grid(row=1, column=4, padx=6)

        # Third row for Manual Mode Controls
        tk.Label(frm, text="Manual Controls").grid(row=2, column=0, padx=4)
        self.manual_frame = tk.Frame(frm)
        self.manual_frame.grid(row=2, column=1, columnspan=4, padx=4)
        
        # Movement buttons in cross pattern
        tk.Button(self.manual_frame, text="↑", width=3, command=lambda: self.on_manual_move("UP")).grid(row=0, column=1, padx=2)
        tk.Button(self.manual_frame, text="←", width=3, command=lambda: self.on_manual_move("LEFT")).grid(row=1, column=0, padx=2)
        tk.Button(self.manual_frame, text="↓", width=3, command=lambda: self.on_manual_move("DOWN")).grid(row=1, column=1, padx=2)
        tk.Button(self.manual_frame, text="→", width=3, command=lambda: self.on_manual_move("RIGHT")).grid(row=1, column=2, padx=2)
        
        # Action buttons
        tk.Button(frm, text="Grab Gold", command=self.on_manual_grab).grid(row=2, column=5, padx=4)
        tk.Button(frm, text="Climb Out", command=self.on_manual_climb).grid(row=2, column=6, padx=4)
        
        # Initially enable manual controls (default mode is Manual)
        self.update_manual_controls_state()

        self.msg_var = tk.StringVar(value="...")
        tk.Label(frm, textvariable=self.msg_var, width=60, anchor="w").grid(row=3, column=0, columnspan=15, sticky="w", padx=6, pady=4)

    def _build_grid(self):
        for child in self.center.winfo_children():
            child.destroy()

        self.buttons = []

        # 1) Make grid cells square & equal sized
        for c in range(self.gs.width):
            self.center.grid_columnconfigure(c, weight=1, uniform="grid")
        for r in range(self.gs.height):
            self.center.grid_rowconfigure(r, weight=1, uniform="grid")

        # 2) Create buttons that FILL their grid cell
        for r in range(self.gs.height):
            row_btns = []
            for c in range(self.gs.width):
                b = tk.Button(
                    self.center,
                    bg="white",
                    borderwidth=1,
                    highlightthickness=0
                )
                b.grid(row=r, column=c, padx=2, pady=2, sticky="nsew")  # clean spacing between cells
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
        
        # Fallback colors
        bg = "white"
        if entity == STAT_HOLE:
            bg = "#b0c4de"
        elif entity == STAT_WUMPUS:
            bg = "#cd5c5c"
        elif entity == STAT_STENCH:
            bg = "#f0e68c"
        elif entity == STAT_WIND:
            bg = "#e0ffff"
        elif entity == STAT_START:
            bg = "#90ee90"
        
        relief = tk.RAISED if STAT_PLAYER in cell.contents else tk.FLAT
        
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
                
                # Fallback colors if no image
                bg = "white"
                if entity == STAT_HOLE:
                    bg = "#b0c4de"
                elif entity == STAT_WUMPUS:
                    bg = "#cd5c5c"
                # elif entity == STAT_GOLD:
                #     bg = "#ffd700"  # Removed gold background color
                # elif entity == STAT_STENCH:
                #     bg = "#f0e68c"
                # elif entity == STAT_WIND:
                #     bg = "#e0ffff"
                elif entity == STAT_START:
                    bg = "#90ee90"
                
                # Player cells get raised border
                relief = tk.RAISED if STAT_PLAYER in cell.contents else tk.FLAT
                
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
        self.msg_var.set(f"Resized to {cols}x{rows}")

    def on_default_setup(self):
        if self.running:
            self.msg_var.set("Stop the game before setup")
            return
        self.gs.setup_default_world()
        self.player_direction = "DOWN"  # Reset player direction to default
        self._color_grid()
        
        # Clear any existing AI path
        if self.prolog_communicator:
            self.prolog_communicator.path_cache = []
            self.prolog_communicator.current_step = 0
        
        self.msg_var.set("Default world: Wumpus(3,4), Gold(6,2), Pits(2,3),(4,1),(1,5)")

    def on_randomize(self):
        if self.running:
            self.msg_var.set("Stop the game before randomize")
            return
        self.gs.randomize(wumpi=1, holes=1)
        self.player_direction = "DOWN"  # Reset player direction to default
        self._color_grid()
        
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
        """Enable/disable manual controls based on current mode"""
        manual_mode = not self.ai_active and not self.running
        
        # Enable/disable movement buttons
        for widget in self.manual_frame.winfo_children():
            if isinstance(widget, tk.Button):
                widget.configure(state="normal" if manual_mode else "disabled")
        
        # Find and update action buttons - search recursively
        def update_buttons_recursive(parent):
            for widget in parent.winfo_children():
                if isinstance(widget, tk.Button):
                    button_text = widget.cget("text")
                    if button_text in ["Grab Gold", "Climb Out"]:
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
        
        if not self.gs.alive:
            self.msg_var.set("💀 You died! Press 'Randomize' to try again.")
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
            if self.gs.has_gold:
                self.msg_var.set("Gold picked up! Return to start (0,0) and press 'c' to climb out.")
        elif key in ['c']:
            self.gs.climb()
            if self.gs.has_escaped:
                self.msg_var.set("🎉 Success! You escaped with the gold!")
        
        if moved:
            self._color_grid()
            if not self.gs.alive:
                self.msg_var.set("💀 You died! Press 'Randomize' to try again.")
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
            if self.gs.has_escaped:
                self.msg_var.set("🎉 Success! Gold collected and escaped! Manual controls restored.")
            elif self.gs.has_gold and self.gs.player_pos == (0, 0):
                self.msg_var.set("🎉 Success! Returned to start with gold! Manual controls restored.")
            else:
                self.msg_var.set("Simulation complete - manual controls restored")
            return
            
        # 3) Refresh UI
        self._color_grid()
        
        # Check if simulation is complete
        if self.gs.has_escaped:
            self.running = False
            self.update_manual_controls_state()
            self.msg_var.set("🎉 Success! Gold collected and escaped!")
        elif not self.gs.alive:
            self.running = False
            self.update_manual_controls_state()
            self.msg_var.set("💀 Game Over! Player died.")

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


if __name__ == "__main__":
    app = WumpusGUI()
    app.mainloop()
