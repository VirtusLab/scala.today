function voting() {
  return {
    votes: JSON.parse(localStorage.getItem('votes')) || [],
    vote(id) {
      if (!this.wasVotedOn(id)) {
        this.votes.push(id);
        // send vote to server (POST, body with {id: id})
        fetch(`/vote`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({ id }),
        }).then(response => {
          // if response is not ok, remove id from votedOn
          if (!response.ok) {
            this.votes = this.votes.filter(votedId => votedId !== id);
          }

          this.saveToLocalStorage();
        });
        this.saveToLocalStorage();

      }
    },
    wasVotedOn(id) {
      return this.votes.includes(id);
    },
    saveToLocalStorage() {
      localStorage.setItem('votes', JSON.stringify(this.votes));
    }
  }
}