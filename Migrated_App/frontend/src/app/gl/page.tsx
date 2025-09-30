'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import { 
  DocumentTextIcon,
  ScaleIcon,
  ChartBarIcon,
  CurrencyDollarIcon,
  CalendarIcon,
  ExclamationTriangleIcon,
  CheckCircleIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Modal from '@/components/UI/Modal'
import Input from '@/components/UI/Input'
import { formatCurrency } from '@/lib/utils'

interface GLSummary {
  total_accounts: number
  trial_balance_status: 'balanced' | 'out_of_balance'
  current_period: string
  period_status: 'open' | 'closed'
  total_debit: number
  total_credit: number
  variance: number
  unposted_journals: number
  pending_approvals: number
}

interface RecentJournal {
  id: number
  journal_number: string
  description: string
  total_amount: number
  entry_date: string
  status: 'posted' | 'pending' | 'draft'
  created_by: string
}

interface TrialBalanceItem {
  account_code: string
  account_name: string
  debit_balance: number
  credit_balance: number
}

export default function GeneralLedgerPage() {
  const router = useRouter()
  const [summary, setSummary] = useState<GLSummary | null>(null)
  const [recentJournals, setRecentJournals] = useState<RecentJournal[]>([])
  const [trialBalancePreview, setTrialBalancePreview] = useState<TrialBalanceItem[]>([])
  const [loading, setLoading] = useState(true)
  const [showJournalModal, setShowJournalModal] = useState(false)
  const [journalForm, setJournalForm] = useState({
    date: new Date().toISOString().split('T')[0],
    reference: '',
    description: '',
    lines: [
      { account_code: '', account_name: '', debit: 0, credit: 0 },
      { account_code: '', account_name: '', debit: 0, credit: 0 }
    ]
  })

  useEffect(() => {
    const fetchData = async () => {
      try {
        // Fetch real data from API
        const summaryResponse = await fetch('http://localhost:8000/api/v1/gl/summary')
        if (summaryResponse.ok) {
          const summaryData = await summaryResponse.json()
          setSummary(summaryData)
        }
        
        const journalsResponse = await fetch('http://localhost:8000/api/v1/gl/recent-journals')
        if (journalsResponse.ok) {
          const journalsData = await journalsResponse.json()
          setRecentJournals(journalsData)
        }
        
        const trialBalanceResponse = await fetch('http://localhost:8000/api/v1/gl/trial-balance-preview')
        if (trialBalanceResponse.ok) {
          const trialBalanceData = await trialBalanceResponse.json()
          setTrialBalancePreview(trialBalanceData)
        }

        // Data is now fetched from API above
      } catch (error) {
        console.error('Failed to fetch GL data:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchData()
  }, [])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm" onClick={() => setShowJournalModal(true)}>
        <DocumentTextIcon className="h-4 w-4" />
        New Journal
      </Button>
      <Button variant="outline" size="sm" onClick={() => router.push('/gl/trial-balance')}>
        <ScaleIcon className="h-4 w-4" />
        Trial Balance
      </Button>
      <Button size="sm" onClick={() => router.push('/reports?module=gl')}>
        <ChartBarIcon className="h-4 w-4" />
        Reports
      </Button>
    </div>
  )


  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-GB', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    })
  }

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'posted':
        return 'bg-green-100 text-green-800'
      case 'pending':
        return 'bg-yellow-100 text-yellow-800'
      case 'draft':
        return 'bg-gray-100 text-gray-800'
      default:
        return 'bg-gray-100 text-gray-800'
    }
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="General Ledger"
        description="Chart of accounts, journal entries, and financial reporting"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'General Ledger' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {summary && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="GL Accounts"
              value={summary.total_accounts.toLocaleString()}
              icon={<DocumentTextIcon className="h-6 w-6" />}
              href="/gl/accounts"
            />
            <StatsCard
              title="Trial Balance"
              value={summary.trial_balance_status === 'balanced' ? 'Balanced' : 'Out of Balance'}
              icon={<ScaleIcon className="h-6 w-6" />}
              change={{ 
                value: formatCurrency(summary.variance), 
                type: summary.variance === 0 ? 'neutral' : 'decrease' 
              }}
              href="/gl/trial-balance"
            />
            <StatsCard
              title="Period Status"
              value={summary.period_status === 'open' ? 'Open' : 'Closed'}
              icon={<CalendarIcon className="h-6 w-6" />}
              href="/gl/periods"
            />
            <StatsCard
              title="Pending Items"
              value={summary.unposted_journals + summary.pending_approvals}
              icon={<ExclamationTriangleIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.pending_approvals} approvals`, 
                type: summary.pending_approvals > 0 ? 'decrease' : 'neutral' 
              }}
              href="/gl/pending"
            />
          </div>
        )}

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Recent Journal Entries */}
          <div className="lg:col-span-2">
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <h3 className="text-lg font-medium text-gray-900">Recent Journal Entries</h3>
                  <Button variant="outline" size="sm" onClick={() => router.push('/gl/journals')}>
                    View All
                  </Button>
                </div>
              </div>
              <div className="p-0">
                {loading ? (
                  <div className="p-6">
                    <div className="animate-pulse space-y-4">
                      {Array.from({ length: 3 }).map((_, index) => (
                        <div key={index} className="flex items-center space-x-4">
                          <div className="h-10 w-10 bg-gray-300 rounded"></div>
                          <div className="flex-1 space-y-2">
                            <div className="h-4 bg-gray-300 rounded w-3/4"></div>
                            <div className="h-3 bg-gray-300 rounded w-1/2"></div>
                          </div>
                        </div>
                      ))}
                    </div>
                  </div>
                ) : (
                  <div className="divide-y divide-gray-200">
                    {recentJournals.map((journal) => (
                      <div key={journal.id} className="p-6 hover:bg-gray-50">
                        <div className="flex items-center justify-between">
                          <div className="flex items-center space-x-3">
                            <div className={`px-2 py-1 text-xs font-medium rounded-full ${getStatusColor(journal.status)}`}>
                              {journal.status.toUpperCase()}
                            </div>
                            <div>
                              <p className="text-sm font-medium text-gray-900">
                                {journal.journal_number}
                              </p>
                              <p className="text-sm text-gray-500">
                                {journal.description}
                              </p>
                              <p className="text-xs text-gray-400">
                                By {journal.created_by}
                              </p>
                            </div>
                          </div>
                          <div className="text-right">
                            <p className="text-sm font-medium text-gray-900">
                              {formatCurrency(journal.total_amount)}
                            </p>
                            <p className="text-xs text-gray-500">
                              {formatDate(journal.entry_date)}
                            </p>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </Card>
          </div>

          {/* Trial Balance Preview & Actions */}
          <div className="space-y-6">
            {/* Period Status */}
            {summary && (
              <Card>
                <div className="px-6 py-4 border-b border-gray-200">
                  <h3 className="text-lg font-medium text-gray-900">Current Period</h3>
                </div>
                <div className="p-6">
                  <div className="space-y-4">
                    <div className="flex items-center justify-between">
                      <span className="text-sm text-gray-500">Period:</span>
                      <span className="text-sm font-medium text-gray-900">{summary.current_period}</span>
                    </div>
                    <div className="flex items-center justify-between">
                      <span className="text-sm text-gray-500">Status:</span>
                      <div className="flex items-center">
                        {summary.period_status === 'open' ? (
                          <CheckCircleIcon className="h-4 w-4 text-green-500 mr-1" />
                        ) : (
                          <ExclamationTriangleIcon className="h-4 w-4 text-red-500 mr-1" />
                        )}
                        <span className={`text-sm font-medium ${
                          summary.period_status === 'open' ? 'text-green-600' : 'text-red-600'
                        }`}>
                          {summary.period_status === 'open' ? 'Open' : 'Closed'}
                        </span>
                      </div>
                    </div>
                    <div className="flex items-center justify-between">
                      <span className="text-sm text-gray-500">Total Debits:</span>
                      <span className="text-sm font-medium text-gray-900">{formatCurrency(summary.total_debit)}</span>
                    </div>
                    <div className="flex items-center justify-between">
                      <span className="text-sm text-gray-500">Total Credits:</span>
                      <span className="text-sm font-medium text-gray-900">{formatCurrency(summary.total_credit)}</span>
                    </div>
                    <div className="pt-2 border-t border-gray-200">
                      <div className="flex items-center justify-between">
                        <span className="text-sm font-medium text-gray-900">Variance:</span>
                        <span className={`text-sm font-bold ${
                          summary.variance === 0 ? 'text-green-600' : 'text-red-600'
                        }`}>
                          {formatCurrency(summary.variance)}
                        </span>
                      </div>
                    </div>
                  </div>
                </div>
              </Card>
            )}

            {/* Quick Actions */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Quick Actions</h3>
              </div>
              <div className="p-6 space-y-3">
                <Button variant="outline" className="w-full justify-start" onClick={() => router.push('/gl/accounts')}>
                  <DocumentTextIcon className="h-4 w-4 mr-2" />
                  Chart of Accounts
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => setShowJournalModal(true)}>
                  <ScaleIcon className="h-4 w-4 mr-2" />
                  Journal Entry
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => router.push('/reports?module=gl')}>
                  <ChartBarIcon className="h-4 w-4 mr-2" />
                  Financial Reports
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => router.push('/gl/periods')}>
                  <CalendarIcon className="h-4 w-4 mr-2" />
                  Period Processing
                </Button>
                <Button variant="outline" className="w-full justify-start" onClick={() => alert('Budget Analysis module coming soon!')}>
                  <CurrencyDollarIcon className="h-4 w-4 mr-2" />
                  Budget Analysis
                </Button>
              </div>
            </Card>

            {/* Trial Balance Preview */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <h3 className="text-lg font-medium text-gray-900">Trial Balance Preview</h3>
                  <Button variant="link" size="sm" onClick={() => router.push('/gl/trial-balance')}>
                    View Full
                  </Button>
                </div>
              </div>
              <div className="p-0">
                {loading ? (
                  <div className="p-6">
                    <div className="animate-pulse space-y-3">
                      {Array.from({ length: 5 }).map((_, index) => (
                        <div key={index} className="flex justify-between">
                          <div className="h-3 bg-gray-300 rounded w-2/3"></div>
                          <div className="h-3 bg-gray-300 rounded w-1/4"></div>
                        </div>
                      ))}
                    </div>
                  </div>
                ) : (
                  <div className="p-6">
                    <div className="space-y-3 text-xs">
                      {trialBalancePreview.map((item) => (
                        <div key={item.account_code} className="flex justify-between items-center">
                          <div className="flex-1">
                            <p className="font-medium text-gray-900">{item.account_code}</p>
                            <p className="text-gray-500 truncate">{item.account_name}</p>
                          </div>
                          <div className="text-right ml-2">
                            {item.debit_balance > 0 ? (
                              <p className="font-medium text-gray-900">
                                Dr {formatCurrency(item.debit_balance)}
                              </p>
                            ) : (
                              <p className="font-medium text-gray-900">
                                Cr {formatCurrency(item.credit_balance)}
                              </p>
                            )}
                          </div>
                        </div>
                      ))}
                      <div className="pt-3 border-t border-gray-200">
                        <div className="flex justify-between">
                          <span className="font-bold text-gray-900">Totals:</span>
                          <span className="font-bold text-green-600">Balanced</span>
                        </div>
                      </div>
                    </div>
                  </div>
                )}
              </div>
            </Card>
          </div>
        </div>

        {/* Alerts */}
        {summary && (summary.unposted_journals > 0 || summary.pending_approvals > 0) && (
          <div className="mt-8">
            <Card>
              <div className="p-6">
                <div className="rounded-md bg-yellow-50 p-4">
                  <div className="flex">
                    <div className="flex-shrink-0">
                      <ExclamationTriangleIcon className="h-5 w-5 text-yellow-400" />
                    </div>
                    <div className="ml-3">
                      <h3 className="text-sm font-medium text-yellow-800">
                        Action Required
                      </h3>
                      <div className="mt-2 text-sm text-yellow-700">
                        <ul className="list-disc pl-5 space-y-1">
                          {summary.unposted_journals > 0 && (
                            <li>{summary.unposted_journals} journal entries awaiting posting</li>
                          )}
                          {summary.pending_approvals > 0 && (
                            <li>{summary.pending_approvals} journal entries awaiting approval</li>
                          )}
                        </ul>
                      </div>
                      <div className="mt-4">
                        <div className="flex space-x-2">
                          {summary.unposted_journals > 0 && (
                            <Button size="sm" variant="outline" onClick={() => router.push('/gl/pending?type=unposted')}>
                              Review Unposted
                            </Button>
                          )}
                          {summary.pending_approvals > 0 && (
                            <Button size="sm" variant="outline" onClick={() => router.push('/gl/pending?type=approval')}>
                              Review Approvals
                            </Button>
                          )}
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </Card>
          </div>
        )}
      </main>

      {/* Journal Entry Modal */}
      <Modal
        isOpen={showJournalModal}
        onClose={() => {
          setShowJournalModal(false)
          setJournalForm({
            date: new Date().toISOString().split('T')[0],
            reference: '',
            description: '',
            lines: [
              { account_code: '', account_name: '', debit: 0, credit: 0 },
              { account_code: '', account_name: '', debit: 0, credit: 0 }
            ]
          })
        }}
        title="New Journal Entry"
        size="xl"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowJournalModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={async () => {
                try {
                  // Calculate totals
                  const totalDebit = journalForm.lines.reduce((sum, line) => sum + (line.debit || 0), 0)
                  const totalCredit = journalForm.lines.reduce((sum, line) => sum + (line.credit || 0), 0)
                  
                  if (totalDebit !== totalCredit) {
                    alert('Journal entry must balance. Total debits must equal total credits.')
                    return
                  }
                  
                  const response = await fetch('http://localhost:8000/api/v1/gl/journal-entry', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                      ...journalForm,
                      total_amount: totalDebit,
                      status: 'pending'
                    })
                  })
                  
                  const data = await response.json()
                  if (data.success) {
                    alert(data.message || 'Journal entry created successfully!')
                    setShowJournalModal(false)
                    window.location.reload() // Refresh to show new journal
                  } else {
                    alert(data.message || 'Failed to create journal entry')
                  }
                } catch (error) {
                  console.error('Error creating journal:', error)
                  alert('Failed to create journal entry')
                }
              }}
            >
              Create Journal
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Input
              label="Date"
              type="date"
              value={journalForm.date}
              onChange={(e) => setJournalForm({...journalForm, date: e.target.value})}
              required
            />
            <Input
              label="Reference"
              type="text"
              value={journalForm.reference}
              onChange={(e) => setJournalForm({...journalForm, reference: e.target.value})}
              placeholder="JV-001"
            />
            <Input
              label="Description"
              type="text"
              value={journalForm.description}
              onChange={(e) => setJournalForm({...journalForm, description: e.target.value})}
              placeholder="Journal description"
              required
            />
          </div>
          
          <div>
            <h4 className="font-medium text-gray-900 mb-2">Journal Lines</h4>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead>
                  <tr>
                    <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase">Account Code</th>
                    <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase">Account Name</th>
                    <th className="px-3 py-2 text-right text-xs font-medium text-gray-500 uppercase">Debit</th>
                    <th className="px-3 py-2 text-right text-xs font-medium text-gray-500 uppercase">Credit</th>
                    <th className="px-3 py-2"></th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-gray-200">
                  {journalForm.lines.map((line, index) => (
                    <tr key={index}>
                      <td className="px-3 py-2">
                        <Input
                          value={line.account_code}
                          onChange={(e) => {
                            const newLines = [...journalForm.lines]
                            newLines[index].account_code = e.target.value
                            setJournalForm({...journalForm, lines: newLines})
                          }}
                          placeholder="1000"
                        />
                      </td>
                      <td className="px-3 py-2">
                        <Input
                          value={line.account_name}
                          onChange={(e) => {
                            const newLines = [...journalForm.lines]
                            newLines[index].account_name = e.target.value
                            setJournalForm({...journalForm, lines: newLines})
                          }}
                          placeholder="Account name"
                        />
                      </td>
                      <td className="px-3 py-2">
                        <Input
                          type="number"
                          value={line.debit}
                          onChange={(e) => {
                            const newLines = [...journalForm.lines]
                            newLines[index].debit = parseFloat(e.target.value) || 0
                            newLines[index].credit = 0 // Clear credit when entering debit
                            setJournalForm({...journalForm, lines: newLines})
                          }}
                          placeholder="0.00"
                          min="0"
                        />
                      </td>
                      <td className="px-3 py-2">
                        <Input
                          type="number"
                          value={line.credit}
                          onChange={(e) => {
                            const newLines = [...journalForm.lines]
                            newLines[index].credit = parseFloat(e.target.value) || 0
                            newLines[index].debit = 0 // Clear debit when entering credit
                            setJournalForm({...journalForm, lines: newLines})
                          }}
                          placeholder="0.00"
                          min="0"
                        />
                      </td>
                      <td className="px-3 py-2">
                        {journalForm.lines.length > 2 && (
                          <Button
                            size="sm"
                            variant="outline"
                            onClick={() => {
                              const newLines = journalForm.lines.filter((_, i) => i !== index)
                              setJournalForm({...journalForm, lines: newLines})
                            }}
                          >
                            Remove
                          </Button>
                        )}
                      </td>
                    </tr>
                  ))}
                </tbody>
                <tfoot>
                  <tr className="bg-gray-50">
                    <td colSpan={2} className="px-3 py-2 text-right font-medium">
                      Totals:
                    </td>
                    <td className="px-3 py-2 text-right font-medium">
                      {formatCurrency(journalForm.lines.reduce((sum, line) => sum + (line.debit || 0), 0))}
                    </td>
                    <td className="px-3 py-2 text-right font-medium">
                      {formatCurrency(journalForm.lines.reduce((sum, line) => sum + (line.credit || 0), 0))}
                    </td>
                    <td></td>
                  </tr>
                </tfoot>
              </table>
            </div>
            <div className="mt-2 flex justify-between">
              <Button
                size="sm"
                variant="outline"
                onClick={() => {
                  setJournalForm({
                    ...journalForm,
                    lines: [...journalForm.lines, { account_code: '', account_name: '', debit: 0, credit: 0 }]
                  })
                }}
              >
                Add Line
              </Button>
              {journalForm.lines.reduce((sum, line) => sum + (line.debit || 0), 0) !== 
               journalForm.lines.reduce((sum, line) => sum + (line.credit || 0), 0) && (
                <span className="text-sm text-red-600 font-medium">
                  Entry does not balance
                </span>
              )}
            </div>
          </div>
        </div>
      </Modal>
    </div>
  )
}