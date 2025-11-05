"""
Prolog Bridge for Wumpus World GUI
This module handles communication between the Python GUI and the Prolog pathfinding system.
"""

import subprocess
import os
import tempfile
import time
from typing import Optional, List, Tuple

class PrologBridge:
    def __init__(self, prolog_file_path: str):
        self.prolog_file = prolog_file_path
        self.temp_dir = tempfile.mkdtemp()
        self.current_algorithm = "bfs"  # Default algorithm
        
    def setup_world_in_prolog(self, width: int, height: int, wumpus_positions: List[Tuple[int, int]], 
                             pit_positions: List[Tuple[int, int]], gold_position: Tuple[int, int]):
        """Setup the world state in Prolog"""
        grid_size = max(width-1, height-1)
        prolog_commands = [
            f"retractall(wumpus(_,_)).",
            f"retractall(pit(_,_)).",
            f"retractall(gold(_,_)).",
            f"retractall(start(_,_)).",
            f"assertz(start(0,0))."
            # Note: Don't modify grid_max as it's defined as static in full_pl.pl
        ]
        
        # Add wumpus positions
        for x, y in wumpus_positions:
            prolog_commands.append(f"assertz(wumpus({x},{y})).")
            
        # Add pit positions  
        for x, y in pit_positions:
            prolog_commands.append(f"assertz(pit({x},{y})).")
            
        # Add gold position
        gx, gy = gold_position
        prolog_commands.append(f"assertz(gold({gx},{gy})).")
        
        return prolog_commands 
    
    def find_path_to_gold(self, algorithm: str = "bfs", world_commands: List[str] = None) -> List[Tuple[int, int]]:
        """Find path to gold using specified algorithm"""
        self.current_algorithm = algorithm
        
        # Create temporary Prolog file with query
        temp_pl_file = os.path.join(self.temp_dir, "temp_query.pl")
        
        with open(temp_pl_file, 'w') as f:
            # Load the main Prolog file
            f.write(f":- consult('{self.prolog_file.replace(chr(92), '/')}').\n")
            
            # Add world setup commands if provided
            if world_commands:
                for cmd in world_commands:
                    f.write(f":- {cmd}\n")
            
            # Query based on algorithm
            if algorithm == "bfs":
                f.write(":- find_gold_path_bfs(Path), print_path(Path), halt.\n")
            elif algorithm == "dfs":
                f.write(":- find_gold_path_dfs(Path), print_path(Path), halt.\n")
            elif algorithm == "astar":
                f.write(":- find_gold_path_astar(Path), print_path(Path), halt.\n")
            elif algorithm == "ids":
                f.write(":- find_gold_path_ids(Path), print_path(Path), halt.\n")
            elif algorithm == "prm":
                f.write(":- find_gold_path_prm(Path), print_path(Path), halt.\n")
            elif algorithm == "ucs":
                f.write(":- find_gold_path_ucs(Path), print_path(Path), halt.\n")
            else:
                f.write(":- find_gold_path_bfs(Path), print_path(Path), halt.\n")
        
        try:
            # Run SWI-Prolog with the temporary file
            result = subprocess.run(['swipl', '-s', temp_pl_file], 
                                  capture_output=True, text=True, timeout=30, 
                                  encoding='utf-8', errors='ignore')
            
            # Uncomment for debugging:
            # print(f"DEBUG: Prolog return code: {result.returncode}")
            # print(f"DEBUG: Prolog stdout: {result.stdout}")
            # print(f"DEBUG: Prolog stderr: {result.stderr}")
            
            if result.returncode == 0 and result.stdout:
                path = self._parse_prolog_output(result.stdout)
                return path
            else:
                print(f"Prolog error: {result.stderr}")
                return []
                
        except subprocess.TimeoutExpired:
            print("Prolog query timed out")
            return []
        except FileNotFoundError:
            print("SWI-Prolog not found. Please install SWI-Prolog and add it to PATH")
            return []
        finally:
            # Clean up temporary file
            if os.path.exists(temp_pl_file):
                os.remove(temp_pl_file)
    
    def _parse_prolog_output(self, output: str) -> List[Tuple[int, int]]:
        """Parse Prolog output to extract path coordinates"""
        if not output:
            return []
            
        path = []
        lines = output.strip().split('\n')
        
        # Uncomment for debugging:
        # print(f"DEBUG: Parsing {len(lines)} lines of output")
        
        for i, line in enumerate(lines):
            # print(f"DEBUG: Line {i}: {line}")
            if '[' in line and ']' in line and '→' in line:
                # Extract coordinates from format like [0,0] → [1,0] → [2,0]
                import re
                coords = re.findall(r'\[(\d+),(\d+)\]', line)
                # print(f"DEBUG: Found coordinates: {coords}")
                
                # Build path from coordinates, removing duplicates
                for x_str, y_str in coords:
                    coord = (int(x_str), int(y_str))
                    if not path or path[-1] != coord:  # Avoid consecutive duplicates
                        path.append(coord)
        
        # Our Prolog algorithms already return path from start to goal, no reversal needed
        # print(f"DEBUG: Final parsed path (start to goal): {path}")           
        return path
    
    def update_world_state(self, game_state):
        """Update Prolog world state from game state"""
        commands = []
        
        # Find wumpus positions
        wumpus_positions = []
        pit_positions = []
        gold_position = None
        
        for y in range(game_state.height):
            for x in range(game_state.width):
                cell = game_state.grid[y][x]
                if "WUMPUS" in cell.contents:
                    wumpus_positions.append((x, y))
                if "HOLE" in cell.contents:
                    pit_positions.append((x, y))
                if "GOLD" in cell.contents:
                    gold_position = (x, y)
        
        if gold_position:
            return self.setup_world_in_prolog(game_state.width, game_state.height, 
                                            wumpus_positions, pit_positions, gold_position)
        return []

class PrologCommunicator:
    """Enhanced communicator that uses Prolog for pathfinding"""
    
    def __init__(self, filename: str, width: int, height: int, prolog_file: str):
        self.filename = filename
        self.prolog_bridge = PrologBridge(prolog_file)
        self.path_cache = []
        self.current_step = 0
        
        # Ensure file exists
        with open(self.filename, "w", encoding="utf-8") as f:
            f.write("")
        # Send INIT
        self.write(f"C;INIT;{width};{height};END")
        
    def write(self, message: str):
        with open(self.filename, "w", encoding="utf-8") as f:
            f.write(message)

    def read(self) -> str:
        try:
            with open(self.filename, "r", encoding="utf-8") as f:
                return f.read()
        except Exception:
            return ""

    def wait_ready(self, timeout_ms: int = 5000) -> bool:
        # Immediately return ready since we're using internal Prolog
        self.write("B;READY")
        return True

    def send_perception(self, gs):
        # Update Prolog world state
        world_commands = self.prolog_bridge.update_world_state(gs)
        
        # Send perception as before
        arr = gs.get_self_status()
        arr_str = "[" + ",".join(arr) + "]" if arr else "[]"
        s = f"C;{arr_str};[0.0,0.0];{str(gs.has_gold).lower()};{str(gs.has_escaped).lower()};{str(gs.alive).lower()}"
        self.write(s)

    def compute_path(self, game_state, algorithm: str = "bfs"):
        """Compute path using Prolog algorithm"""
        # Update world state first
        world_commands = self.prolog_bridge.update_world_state(game_state)
        
        # Get path from Prolog
        path = self.prolog_bridge.find_path_to_gold(algorithm, world_commands)
        
        print(f"DEBUG: Computed path: {path}")
        print(f"DEBUG: Player current position: {game_state.player_pos}")
        
        if path and len(path) > 0:
            self.path_cache = path
            self.current_step = 0
            
            # Ensure path starts from current player position
            if path[0] != game_state.player_pos:
                print(f"DEBUG: Warning - path starts at {path[0]} but player is at {game_state.player_pos}")
            
            return True
        return False

    def get_next_command(self) -> Optional[str]:
        """Get next command based on computed path"""
        if not self.path_cache:
            return None
            
        # If we're at or past the end of the path
        if self.current_step >= len(self.path_cache):
            return None
            
        # If we're at the last position in path (gold location)
        if self.current_step == len(self.path_cache) - 1:
            self.current_step += 1  # Mark as done
            # Don't move when picking up gold - use UP as a no-op since we'll handle pickup first
            return "B;UP;false;true;false"  # Pickup gold but don't climb yet (need to return to start)
        
        current_pos = self.path_cache[self.current_step]
        next_pos = self.path_cache[self.current_step + 1]
        
        dx = next_pos[0] - current_pos[0]
        dy = next_pos[1] - current_pos[1]
        
        # Convert to movement command
        if dx == 1:
            move = "RIGHT"
        elif dx == -1:
            move = "LEFT"
        elif dy == 1:
            move = "DOWN"
        elif dy == -1:
            move = "UP"
        else:
            move = "UP"  # Default fallback
            
        self.current_step += 1
        
        # Check if next position is gold location
        pickup = "true" if self.current_step == len(self.path_cache) - 1 else "false"
        
        command = f"B;{move};false;{pickup};false"
        return command

    def read_command(self, timeout_ms: int = 3000) -> Optional[str]:
        # Return next command from computed path
        return self.get_next_command()