import sqlite3
import os
import email
from email import policy
from email.parser import BytesParser

# Function to parse .eml file
def parse_eml(file_path):
    with open(file_path, 'rb') as f:
        msg = BytesParser(policy=policy.default).parse(f)
    return {
        'sender': msg['from'],
        'receiver': msg['to'],
        'subject': msg['subject'],
        'body': msg.get_body(preferencelist=('plain', 'html')).get_content(),
        'timestamp': msg['date']
    }

# Connect to SQLite database
conn = sqlite3.connect('myEmails.db')
cursor = conn.cursor()

# Directory containing your .eml files
eml_directory = 'C:/Users/User/Desktop/database'
for file_name in os.listdir(eml_directory):
    if file_name.endswith('.eml'):
        email_data = parse_eml(os.path.join(eml_directory, file_name))

        # Prompt for the Schwierigkeit value
        schwierigkeit = input(f"Enter Schwierigkeit for email {file_name}: ")

        cursor.execute("INSERT INTO emails (sender, receiver, subject, body, timestamp, Schwierigkeit) VALUES (?, ?, ?, ?, ?, ?)",
                       (email_data['sender'], email_data['receiver'], email_data['subject'], email_data['body'], email_data['timestamp'], schwierigkeit))

# Commit changes and close
conn.commit()
conn.close()
