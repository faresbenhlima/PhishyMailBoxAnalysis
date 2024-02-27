import sqlite3
import csv  # or json, depending on your data format

# Connect to SQLite database
conn = sqlite3.connect('myEmails.db')
cursor = conn.cursor()

# Open your CSV file
with open('emails.csv', 'r') as file:
    reader = csv.DictReader(file)  # or use json.load(file) for JSON

    for row in reader:
        # Extract data from each row
        sender = row['sender']
        receiver = row['receiver']
        subject = row['subject']
        body = row['body']
        timestamp = row['timestamp']

        # Insert data into the database
        cursor.execute("INSERT INTO emails (sender, receiver, subject, body, timestamp) VALUES (?, ?, ?, ?, ?)",
                       (sender, receiver, subject, body, timestamp))

# Commit changes and close
conn.commit()
conn.close()
