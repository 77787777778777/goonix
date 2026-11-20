// goonix: asynchronous, untrusted cache graph query.  Never fails its waiter.
class GoonixGraphGoal : public Goal
{
    friend class Worker;
    Path drvPath;
    std::shared_ptr<Agent> agent;
    string input;
    bool eof = false;
    Strings caches;
    Strings::const_iterator next;
    typedef void (GoonixGraphGoal::*GoalState)();
    GoalState state;
    bool waiting = false;
public:
    GoonixGraphGoal(const Path & path, Worker & worker);
    ~GoonixGraphGoal();
    void work() override;
    void timedOut() override;
    void handleChildOutput(int fd, const string & data) override;
    void handleEOF(int fd) override;
    string key() override { return "0$goonix-graph$" + drvPath; }
private:
    void start();
    void retry(const string & reason);
    void finish();
    void finishWithoutFailure(const string & reason);
};
