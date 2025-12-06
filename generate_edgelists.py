#!/usr/bin/env python3
"""
Edge List Generation Script for SNA Project

This script generates all edge lists required for the Social Network Analysis project:
1. Voting agreement edge lists (pre-election and post-formation)
2. Co-sponsorship edge lists (pre-election and post-formation)
3. Coalition edge lists (processed from CSV)

All edge lists are saved to the results/edge_lists/ directory for use by the R analysis.
"""

import pandas as pd
import json
from datetime import datetime
import os
from collections import defaultdict
import sys

def setup_directories():
    """Create necessary output directories"""
    os.makedirs("results/edge_lists", exist_ok=True)
    print("Output directories created/verified")

def load_voting_data():
    """Load and process voting data from separate files"""
    print("Loading voting data...")
    
    # Load pre-election data (2023)
    voting_data_2023 = pd.read_csv("data/voting_data_2023_preelection.csv")
    voting_data_2023['GewijzigdOp'] = pd.to_datetime(voting_data_2023['GewijzigdOp'], format='mixed')
    
    # Load post-formation data (from voting_data_clean.csv)
    voting_data_clean = pd.read_csv("data/voting_data_clean.csv")
    voting_data_clean['GewijzigdOp'] = pd.to_datetime(voting_data_clean['GewijzigdOp'], format='mixed')
    
    # Define date ranges (matching original R analysis)
    # Pre-election: 1 year before election (Nov 22, 2022 - Nov 21, 2023)
    election_date = pd.Timestamp('2023-11-22', tz='UTC')
    pre_election_start = election_date - pd.DateOffset(years=1)
    pre_election_end = election_date - pd.DateOffset(days=1)
    
    # Post-formation: 1 year after formation (Jul 5, 2024 onwards)
    formation_date = pd.Timestamp('2024-07-05', tz='UTC')
    post_formation_start = formation_date
    
    # Filter to correct date ranges
    motion_data_pre = voting_data_2023[
        (voting_data_2023['GewijzigdOp'] >= pre_election_start) & 
        (voting_data_2023['GewijzigdOp'] <= pre_election_end)
    ].copy()
    
    motion_data_post = voting_data_clean[
        voting_data_clean['GewijzigdOp'] >= post_formation_start
    ].copy()
    
    print(f"Pre-election voting records: {len(motion_data_pre):,}")
    print(f"Post-formation voting records: {len(motion_data_post):,}")
    
    return motion_data_pre, motion_data_post

def generate_voting_edgelist(data, period_name):
    """
    Generate edge list from voting data based on voting agreement
    
    Args:
        data: DataFrame with columns Besluit_Id, ActorFractie, Soort
        period_name: String identifier for the period (for logging)
    
    Returns:
        DataFrame with columns from, to, weight (agreement count)
    """
    print(f"Generating voting edge list for {period_name}...")
    
    # Remove duplicates
    party_votes = data[['ActorFractie', 'Besluit_Id', 'Soort']].drop_duplicates()
    
    agreements_dict = defaultdict(lambda: {'total_votes': 0, 'agreements': 0})
    unique_motions = party_votes['Besluit_Id'].unique()
    
    for motion in unique_motions:
        motion_data = party_votes[party_votes['Besluit_Id'] == motion]
        parties = motion_data['ActorFractie'].tolist()
        votes = motion_data['Soort'].tolist()
        
        if len(parties) >= 2:
            for i in range(len(parties)):
                for j in range(i + 1, len(parties)):
                    party1, party2 = parties[i], parties[j]
                    vote1, vote2 = votes[i], votes[j]
                    
                    # Create canonical pair (alphabetical order)
                    if party1 < party2:
                        pair_key = (party1, party2)
                    else:
                        pair_key = (party2, party1)
                    
                    # Count agreement (1 if same vote, 0 if different)
                    agreement = 1 if vote1 == vote2 else 0
                    
                    agreements_dict[pair_key]['total_votes'] += 1
                    agreements_dict[pair_key]['agreements'] += agreement
    
    # Convert to DataFrame
    if agreements_dict:
        edgelist_data = []
        for (party1, party2), counts in agreements_dict.items():
            # Calculate agreement rate as fraction (0-1)
            agreement_rate = counts['agreements'] / counts['total_votes'] if counts['total_votes'] > 0 else 0
            edgelist_data.append({
                'from': party1,
                'to': party2,
                'weight': agreement_rate
            })
        
        edgelist = pd.DataFrame(edgelist_data)
    else:
        edgelist = pd.DataFrame(columns=['from', 'to', 'weight'])
    
    print(f"  Generated {len(edgelist)} edges for {period_name}")
    return edgelist

def load_coauthoring_data():
    """Load co-authoring data for both periods"""
    print("Loading co-authoring data...")
    
    # Load pre-election data
    with open("data/coauthoring_data_2023_preelection.json", 'r') as f:
        doc_actor_pre = json.load(f)
    
    # Load post-formation data  
    with open("data/coauthoring_data_2024_postformation.json", 'r') as f:
        doc_actor_post = json.load(f)
    
    print(f"Pre-election documents: {len(doc_actor_pre):,}")
    print(f"Post-formation documents: {len(doc_actor_post):,}")
    
    return doc_actor_pre, doc_actor_post

def generate_cosponsor_edgelist(doc_actor_data, period_name):
    """
    Generate co-sponsorship edge list from document-actor data
    
    Args:
        doc_actor_data: List of documents with DocumentActor information
        period_name: String identifier for the period
    
    Returns:
        DataFrame with columns from, to, weight (co-sponsorship count)
    """
    print(f"Generating co-sponsorship edge list for {period_name}...")
    
    # Extract co-signers and sponsors for each document
    cosponsor_pairs = []
    
    for doc in doc_actor_data:
        doc_id = doc['Id']
        actors = doc.get('DocumentActor', [])
        
        # Separate sponsors and cosigners
        sponsors = []
        cosigners = []
        
        for actor in actors:
            relatie = str(actor.get('Relatie', '')).strip().lower()
            if relatie == 'eerste ondertekenaar':
                sponsors.append(actor.get('ActorFractie'))
            elif relatie == 'mede ondertekenaar':
                cosigners.append(actor.get('ActorFractie'))
        
        # Create pairs between cosigners and sponsors
        for cosigner in cosigners:
            for sponsor in sponsors:
                if cosigner != sponsor and cosigner and sponsor:
                    # Create canonical pair (alphabetical order)
                    if cosigner < sponsor:
                        pair = (cosigner, sponsor)
                    else:
                        pair = (sponsor, cosigner)
                    cosponsor_pairs.append(pair)
    
    # Count co-sponsorship occurrences
    if cosponsor_pairs:
        pair_counts = defaultdict(int)
        for pair in cosponsor_pairs:
            pair_counts[pair] += 1
        
        # Convert to DataFrame
        edgelist_data = []
        for (party1, party2), count in pair_counts.items():
            edgelist_data.append({
                'from': party1,
                'to': party2,
                'weight': count
            })
        
        edgelist = pd.DataFrame(edgelist_data)
    else:
        edgelist = pd.DataFrame(columns=['from', 'to', 'weight'])
    
    print(f"  Generated {len(edgelist)} co-sponsorship edges for {period_name}")
    return edgelist

def load_and_process_coalition_data():
    """Load and process coalition data from CSV"""
    print("Loading coalition data...")
    
    coalition_data = pd.read_csv("data/nrtimes_coalition_together.csv")
    
    # Handle the specific CSV format where all data is in one column
    if len(coalition_data.columns) == 1:
        col_name = coalition_data.columns[0]
        if 'Source,Target,Weight' in col_name:
            # Parse the CSV data that's embedded in the column name and values
            rows = []
            for idx, row in coalition_data.iterrows():
                row_str = str(row.iloc[0])
                if ',' in row_str:
                    parts = row_str.split(',')
                    if len(parts) >= 3:
                        rows.append({
                            'from': parts[0].strip().strip('"'),
                            'to': parts[1].strip().strip('"'),
                            'weight': int(parts[2].strip().strip('"'))
                        })
            
            coalition_edgelist = pd.DataFrame(rows)
        else:
            coalition_edgelist = pd.DataFrame(columns=['from', 'to', 'weight'])
    else:
        # Standard CSV format
        coalition_edgelist = coalition_data.copy()
        
        # Standardize column names
        column_mapping = {
            'Source': 'from',
            'Target': 'to', 
            'Weight': 'weight'
        }
        coalition_edgelist = coalition_edgelist.rename(columns=column_mapping)
    
    print(f"  Loaded {len(coalition_edgelist)} coalition edges")
    return coalition_edgelist

def filter_edgelist_by_parties(edgelist, valid_parties, edgelist_name):
    """Filter edge list to only include valid parties"""
    if len(edgelist) == 0:
        return edgelist
    
    original_count = len(edgelist)
    filtered = edgelist[
        edgelist['from'].isin(valid_parties) & 
        edgelist['to'].isin(valid_parties)
    ].copy()
    
    print(f"  {edgelist_name}: {original_count} -> {len(filtered)} edges after filtering")
    return filtered

def save_edgelist(edgelist, filename):
    """Save edge list to CSV file"""
    filepath = os.path.join("results/edge_lists", filename)
    edgelist.to_csv(filepath, index=False)
    print(f"  Saved: {filepath}")

def main():
    """Main execution function"""
    print("="*60)
    print("EDGE LIST GENERATION FOR SNA PROJECT")
    print("="*60)
    print()
    
    # Setup
    setup_directories()
    print()
    
    # Load ideology data to get valid parties
    ideology_data = pd.read_csv("data/political_axes_data.csv")
    ideology_data.columns = ["left_right", "conservative_progressive", "party"]
    ideology_data = ideology_data.dropna(subset=['party'])
    ideology_data = ideology_data[ideology_data['party'] != '']
    valid_parties = list(ideology_data['party'])
    
    print(f"Valid parties from ideology data: {len(valid_parties)}")
    print()
    
    # 1. Generate voting agreement edge lists
    print("1. VOTING AGREEMENT EDGE LISTS")
    print("-" * 40)
    motion_data_pre, motion_data_post = load_voting_data()
    
    # Filter to valid parties BEFORE generating edge lists (like R code)
    print("Filtering motion data to valid parties...")
    motion_data_pre_filtered = motion_data_pre[motion_data_pre['ActorFractie'].isin(valid_parties)].copy()
    motion_data_post_filtered = motion_data_post[motion_data_post['ActorFractie'].isin(valid_parties)].copy()
    
    print(f"  Pre-election: {len(motion_data_pre):,} -> {len(motion_data_pre_filtered):,} records")
    print(f"  Post-formation: {len(motion_data_post):,} -> {len(motion_data_post_filtered):,} records")
    
    edgelist_pre = generate_voting_edgelist(motion_data_pre_filtered, "pre-election")
    edgelist_post = generate_voting_edgelist(motion_data_post_filtered, "post-formation")
    
    save_edgelist(edgelist_pre, "edges_pre_election.csv")
    save_edgelist(edgelist_post, "edges_post_formation.csv")
    print()
    
    # 2. Generate co-sponsorship edge lists
    print("2. CO-SPONSORSHIP EDGE LISTS")
    print("-" * 40)
    doc_actor_pre, doc_actor_post = load_coauthoring_data()
    
    cosponsor_pre = generate_cosponsor_edgelist(doc_actor_pre, "pre-election")
    cosponsor_post = generate_cosponsor_edgelist(doc_actor_post, "post-formation")
    
    save_edgelist(cosponsor_pre, "cosponsor_pre_election.csv")
    save_edgelist(cosponsor_post, "cosponsor_post_formation.csv")
    print()
    
    # 3. Process coalition edge lists
    print("3. COALITION EDGE LISTS")
    print("-" * 40)
    coalition_edgelist = load_and_process_coalition_data()
    save_edgelist(coalition_edgelist, "coalition_edges.csv")
    print()
    
    # 4. Create filtered versions for studies
    print("4. FILTERED EDGE LISTS FOR STUDIES")
    print("-" * 40)
    
    # Define party removal strategy (matching R script logic)
    parties_to_remove_general = ["Omtzigt", "BIJ1", "BVNL", "50PLUS"]
    nodes_to_remove_pre_additional = ["GroenLinks-PvdA", "NSC"]
    nodes_to_remove_post_additional = ["GroenLinks", "PvdA"]
    
    # Complete removal lists for Study 2
    nodes_to_remove_pre_study2 = parties_to_remove_general + nodes_to_remove_pre_additional
    nodes_to_remove_post_study2 = parties_to_remove_general + nodes_to_remove_post_additional
    
    # Study 1 parties (remove general parties only)
    study1_parties = [p for p in valid_parties if p not in parties_to_remove_general]
    
    # Study 2 parties (different per period)
    study2_parties_pre = [p for p in valid_parties if p not in nodes_to_remove_pre_study2]
    study2_parties_post = [p for p in valid_parties if p not in nodes_to_remove_post_study2]
    
    print("PARTY FILTERING STRATEGY:")
    print("=========================")
    print(f"General removal (both studies): {', '.join(parties_to_remove_general)}")
    print(f"Additional Study 2 pre-election: {', '.join(nodes_to_remove_pre_additional)}")
    print(f"Additional Study 2 post-formation: {', '.join(nodes_to_remove_post_additional)}")
    print()
    print(f"Study 1: {len(study1_parties)} nodes (identical structure)")
    print(f"Study 2: Pre({len(study2_parties_pre)} nodes), Post({len(study2_parties_post)} nodes)")
    print()
    
    # Study 1 versions (identical node structure)
    study1_pre = filter_edgelist_by_parties(edgelist_pre, study1_parties, "Study 1 voting pre")
    study1_post = filter_edgelist_by_parties(edgelist_post, study1_parties, "Study 1 voting post")
    study1_cosponsor_pre = filter_edgelist_by_parties(cosponsor_pre, study1_parties, "Study 1 co-sponsor pre")
    study1_cosponsor_post = filter_edgelist_by_parties(cosponsor_post, study1_parties, "Study 1 co-sponsor post")
    study1_coalition = filter_edgelist_by_parties(coalition_edgelist, study1_parties, "Study 1 coalition")
    
    save_edgelist(study1_pre, "study1_edges_pre_election.csv")
    save_edgelist(study1_post, "study1_edges_post_formation.csv")
    save_edgelist(study1_cosponsor_pre, "study1_cosponsor_pre_election.csv")
    save_edgelist(study1_cosponsor_post, "study1_cosponsor_post_formation.csv")
    save_edgelist(study1_coalition, "study1_coalition_edges.csv")
    
    # Study 2 versions (optimized structure per period)
    study2_voting_pre = filter_edgelist_by_parties(edgelist_pre, study2_parties_pre, "Study 2 voting pre")
    study2_voting_post = filter_edgelist_by_parties(edgelist_post, study2_parties_post, "Study 2 voting post")
    study2_cosponsor_pre = filter_edgelist_by_parties(cosponsor_pre, study2_parties_pre, "Study 2 co-sponsor pre")
    study2_cosponsor_post = filter_edgelist_by_parties(cosponsor_post, study2_parties_post, "Study 2 co-sponsor post") 
    study2_coalition_pre = filter_edgelist_by_parties(coalition_edgelist, study2_parties_pre, "Study 2 coalition pre")
    study2_coalition_post = filter_edgelist_by_parties(coalition_edgelist, study2_parties_post, "Study 2 coalition post")
    
    save_edgelist(study2_voting_pre, "study2_edges_pre_election.csv")
    save_edgelist(study2_voting_post, "study2_edges_post_formation.csv")
    save_edgelist(study2_cosponsor_pre, "study2_cosponsor_pre_election.csv")
    save_edgelist(study2_cosponsor_post, "study2_cosponsor_post_formation.csv")
    save_edgelist(study2_coalition_pre, "study2_coalition_pre_election.csv")
    save_edgelist(study2_coalition_post, "study2_coalition_post_formation.csv")
    print()
    
    print("="*60)
    print("EDGE LIST GENERATION COMPLETED SUCCESSFULLY")
    print("="*60)
    print()
    print("Generated files:")
    print("BASE EDGE LISTS:")
    print("  • edges_pre_election.csv - Voting agreements (pre-election)")
    print("  • edges_post_formation.csv - Voting agreements (post-formation)")
    print("  • cosponsor_pre_election.csv - Co-sponsorships (pre-election)")
    print("  • cosponsor_post_formation.csv - Co-sponsorships (post-formation)")
    print("  • coalition_edges.csv - Coalition relationships")
    print()
    print("STUDY 1 (QAP) - Identical node structure:")
    print("  • study1_edges_*.csv - Voting agreements")
    print("  • study1_cosponsor_*.csv - Co-sponsorships") 
    print("  • study1_coalition_edges.csv - Coalition relationships")
    print()
    print("STUDY 2 (ERGM) - Optimized structure per period:")
    print("  • study2_edges_*.csv - Voting agreements")
    print("  • study2_cosponsor_*.csv - Co-sponsorships")
    print("  • study2_coalition_*.csv - Coalition relationships")
    print()
    print("All files saved to: results/edge_lists/")

if __name__ == "__main__":
    try:
        main()
    except FileNotFoundError as e:
        print(f"Error: Required data file not found - {e}")
        print("Make sure you're running this script from the project root directory")
        sys.exit(1)
    except Exception as e:
        print(f"Error during edge list generation: {e}")
        sys.exit(1)
